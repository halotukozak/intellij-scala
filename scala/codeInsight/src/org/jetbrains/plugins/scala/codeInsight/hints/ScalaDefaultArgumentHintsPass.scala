package org.jetbrains.plugins.scala.codeInsight.hints

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.text.StringUtil
import com.intellij.pom.Navigatable
import com.intellij.psi.{PsiElement, PsiFile}
import org.jetbrains.plugins.scala.annotator.hints.Hint.HintPosition
import org.jetbrains.plugins.scala.annotator.hints.{Hint, Text}
import org.jetbrains.plugins.scala.editor.documentationProvider.ScalaDocQuickInfoGenerator
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.incremental.Highlighting.ElementHighlightingExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructorInvocation, ScInterpolatedStringLiteral, ScLiteral, ScMethodLike}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.settings.ScalaApplicationSettings.{getInstance => ScalaApplicationSettings}

import scala.annotation.tailrec

/**
 * X-Ray hints for default arguments, SCL-25804.
 *
 * Like an implicit argument, an omitted default argument is synthesized by the compiler, so in X-Ray mode
 * we show it at the end of the argument list:
 * {{{
 *   def log(message: String, level: String = "INFO") = ???
 *
 *   log("System starting"/*<, "INFO">*/)
 * }}}
 *
 * The default expression is shown as is only when it means the same at the call site: a literal, or a reference
 * to an accessible symbol which resolves to the same target there. Otherwise (a complex expression, a private
 * member, a reference to another parameter, ...) the hint reads `default$n`, the way the compiler refers to the
 * n-th default argument getter of the method (similarly to `evidence$n` of context bounds).
 *
 * The argument is prefixed with the parameter name when a positional argument is impossible (the call already
 * uses named arguments) or when parameter name hints are shown. Each part navigates to the parameter or to its
 * default expression respectively.
 */
private[codeInsight] trait ScalaDefaultArgumentHintsPass {
  import ScalaDefaultArgumentHintsPass._

  protected implicit def settings: ScalaHintsSettings

  protected def collectDefaultArgumentHints(editor: Editor, root: PsiFile): Seq[Hint] =
    if (!(ScalaHintsSettings.xRayMode && ScalaApplicationSettings.XRAY_SHOW_DEFAULT_ARGUMENT_HINTS)) Seq.empty
    else {
      val showNames = settings.showParameters
      root.elements(_.isVisible(editor.getProject, root)).flatMap {
        case args: ScArgumentExprList if args.isArgsInParens => hintForArguments(args, showNames)
        case invocation: ScConstructorInvocation if invocation.arguments.isEmpty => hintForConstructorWithoutArguments(invocation, showNames)
        case _ => None
      }.toSeq
    }
}

private[codeInsight] object ScalaDefaultArgumentHintsPass {

  /** Maximum length of a rendered default expression; longer expressions are truncated with an ellipsis. */
  private[hints] final val MaxValueLength = 20

  private def hintForArguments(args: ScArgumentExprList, showNames: Boolean): Option[Hint] = {
    val closingParen = args.getLastChild
    if (closingParen == null || closingParen.elementType != ScalaTokenTypes.tRPARENTHESIS) return None

    for {
      (method, clause) <- targetClause(args)
      omitted = omittedDefaultParameters(args, clause)
      if omitted.nonEmpty
    } yield {
      val usesNamedArguments = args.exprs.exists(isNamedArgumentOf(clause))
      val afterComma = closingParen.prevSiblingNotWhitespaceComment.exists(_.elementType == ScalaTokenTypes.tCOMMA)
      val prefix = if (args.exprs.isEmpty || afterComma) Seq.empty else Seq(Text(", "))

      val arguments = omitted.map(argumentPresentation(_, method, args, usesNamedArguments || showNames))
      Hint(prefix ++ arguments.intersperse(Seq(Text(", "))).flatten, closingParen, position = HintPosition.BeforeElement)
    }
  }

  /** `new Foo` or `extends Foo`, where `class Foo(x: Int = 0)` */
  private def hintForConstructorWithoutArguments(invocation: ScConstructorInvocation, showNames: Boolean): Option[Hint] =
    for {
      constructor <- invocation.reference.flatMap(_.resolve().toOption).flatMap(toMethodLike)
      clause <- constructor.effectiveParameterClauses.headOption
      if !clause.isImplicit
      parameters = clause.effectiveParameters
      if parameters.nonEmpty && parameters.forall(p => p.isDefaultParam || p.isRepeatedParameter)
      omitted = parameters.filterNot(_.isRepeatedParameter)
      if omitted.nonEmpty
    } yield {
      val arguments = omitted.map(argumentPresentation(_, constructor, invocation, showNames))
      Hint(
        (Text("(") +: arguments.intersperse(Seq(Text(", "))).flatten) :+ Text(")"),
        invocation,
        position = HintPosition.AfterElement,
        relatesToPrecedingElement = true
      )
    }

  private def argumentPresentation(parameter: ScParameter, method: ScMethodLike, place: PsiElement, named: Boolean): Seq[Text] = {
    val name =
      if (named) {
        val tooltip = () => ScalaDocQuickInfoGenerator.getQuickNavigateInfo(parameter, place)
        Seq(Text(parameter.name, navigatable = Some(parameter), tooltip = tooltip), Text(s" ${ScalaTokenTypes.tASSIGN} "))
      } else Seq.empty

    val value = parameter.getDefaultExpression match {
      case Some(expression) =>
        val text =
          if (meansTheSameAt(expression, place)) presentableText(expression)
          else s"default$$${defaultGetterIndex(parameter, method)}"
        val fullText = normalized(expression.getText)
        val tooltip = () => Option.when(text != fullText)(StringUtil.escapeXmlEntities(fullText))
        Text(text, navigatable = expression.asOptionOfUnsafe[Navigatable], tooltip = tooltip)
      case None =>
        Text(s"default$$${defaultGetterIndex(parameter, method)}")
    }

    name :+ value
  }

  private[hints] def presentableText(expression: ScExpression): String = {
    val text = normalized(expression.getText)
    if (text.length <= MaxValueLength) text
    else text.take(MaxValueLength - 1) + "…"
  }

  private def normalized(text: String): String = text.replaceAll("\\s+", " ").trim

  /** Index of the parameter among all the parameters of the method, the `n` in the `method$default$n` getter. */
  private def defaultGetterIndex(parameter: ScParameter, method: ScMethodLike): Int =
    method.effectiveParameterClauses.flatMap(_.effectiveParameters).indexOf(parameter) + 1

  /** Whether the default expression can be written at `place` with the same meaning. */
  private def meansTheSameAt(expression: ScExpression, place: PsiElement): Boolean = expression match {
    case _: ScInterpolatedStringLiteral => false
    case _: ScLiteral | _: ScUnitExpr => true
    case ScPrefixExpr(operation, literal: ScLiteral) if !literal.is[ScInterpolatedStringLiteral] =>
      operation.refName == "-" || operation.refName == "+"
    case reference: ScReferenceExpression if isStableReference(reference) =>
      val target = reference.resolve()
      target != null && {
        val copy = ScalaPsiElementFactory.createExpressionWithContextFromText(reference.getText, place.getContext, place)
        copy match {
          case ref: ScReferenceExpression => ref.bind().exists(r => r.isAccessible && r.element == target)
          case _ => false
        }
      }
    case _ => false
  }

  @tailrec
  private def isStableReference(reference: ScReferenceExpression): Boolean = reference.qualifier match {
    case None => true
    case Some(qualifier: ScReferenceExpression) => isStableReference(qualifier)
    case Some(_) => false
  }

  private def isNamedArgumentOf(clause: Seq[ScParameter])(argument: ScExpression): Boolean = argument match {
    case ScAssignment(ref: ScReferenceExpression, _) => ref.qualifier.isEmpty && clause.exists(_.name == ref.refName)
    case _ => false
  }

  /** Default parameters of the clause corresponding to `args` which are not passed explicitly. */
  private def omittedDefaultParameters(args: ScArgumentExprList, clause: Seq[ScParameter]): Seq[ScParameter] = {
    if (!clause.exists(_.isDefaultParam)) return Seq.empty

    val arguments = args.exprs
    val namedArguments = arguments.collect {
      case ScAssignment(ref: ScReferenceExpression, _) if ref.qualifier.isEmpty => ref.refName
    }.toSet
    val positionalCount = arguments.takeWhile(!isNamedArgumentOf(clause)(_)).size

    clause.zipWithIndex.collect {
      case (parameter, index)
        if index >= positionalCount &&
          !parameter.isRepeatedParameter &&
          !namedArguments.contains(parameter.name) &&
          parameter.isDefaultParam => parameter
    }
  }

  private def targetClause(args: ScArgumentExprList): Option[(ScMethodLike, Seq[ScParameter])] = {
    val target = args.getContext match {
      case call: ScMethodCall if call.args == args =>
        methodCallTarget(call, clauseIndex = 0)
      case invocation: ScConstructorInvocation =>
        val index = invocation.arguments.indexOf(args)
        invocation.reference.flatMap(_.resolve().toOption).flatMap(toMethodLike).map(_ -> index)
      case _ => None
    }

    target.collect {
      case (method, index) if method.effectiveParameterClauses.indices.contains(index) =>
        (method, method.effectiveParameterClauses(index))
    }.filterNot { case (_, clause) => clause.isImplicit && !args.isUsing }
      .map { case (method, clause) => (method, clause.effectiveParameters) }
  }

  @tailrec
  private def methodCallTarget(call: ScMethodCall, clauseIndex: Int): Option[(ScMethodLike, Int)] =
    call.applyOrUpdateElement match {
      case Some(resolveResult) =>
        toMethodLike(resolveResult.element).map(_ -> clauseIndex)
      case None =>
        call.getEffectiveInvokedExpr match {
          case inner: ScMethodCall =>
            methodCallTarget(inner, clauseIndex + 1)
          case invoked =>
            val reference = invoked match {
              case generic: ScGenericCall => generic.referencedExpr
              case other => other
            }
            reference match {
              case ref: ScReferenceExpression => ref.resolve().toOption.flatMap(toMethodLike).map(_ -> clauseIndex)
              case _ => None
            }
        }
    }

  private def toMethodLike(element: PsiElement): Option[ScMethodLike] = element match {
    case function: ScFunction if function.isApplyMethod && function.syntheticCaseClass.isDefined =>
      function.syntheticCaseClass.flatMap(_.constructor)
    case function: ScFunction if function.isCopyMethod && !function.isPhysical =>
      // defaults of a synthetic case class `copy` are just the current field values
      None
    case method: ScMethodLike => Some(method)
    case cls: ScClass => cls.constructor
    case _ => None
  }
}
