package org.jetbrains.plugins.scala.codeInsight.hints.lambdas

import com.intellij.codeInsight.hints.declarative._
import com.intellij.openapi.editor.Editor
import com.intellij.psi.{PsiElement, PsiFile, SmartPointerManager}
import org.jetbrains.plugins.scala.codeInsight.hints.lambdas.ScalaLambdaReturnHintsProvider._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.types.Context
import org.jetbrains.plugins.scala.lang.psi.types.api.{FunctionType, PartialFunctionType}

/**
 * Marks the expression which is the result of a multi-statement lambda, similar to Kotlin's `^label` hints:
 * {{{
 *   xs.map { x =>
 *     val y = x * 2
 *     y + 1/*<# ^map #>*/
 *   }
 * }}}
 *
 * Applies to function literals, pattern-matching anonymous functions (per case clause)
 * and blocks passed to by-name parameters, but only when the body consists of more than one statement.
 * Lambdas whose result is `Unit` or is discarded (expected result type `Unit`) are skipped.
 * If the result is an `if` spanning several lines, a `match` or a `try`, every branch result is marked instead.
 * The label navigates to the lambda.
 */
final class ScalaLambdaReturnHintsProvider extends InlayHintsProvider {
  override def createCollector(file: PsiFile, editor: Editor): InlayHintsCollector = Collector
}

object ScalaLambdaReturnHintsProvider {
  final val ProviderId = "scala.lambda.return.hints"

  private final val DefaultLabel = "lambda"

  private object Collector extends SharedBypassCollector {
    override def collectFromElement(element: PsiElement, sink: InlayTreeSink): Unit = element match {
      case function: ScFunctionExpr =>
        for {
          body <- function.result
          if isMultiStatement(body)
        } addHints(lambdaElement(function), body, sink)
      case block: ScBlockExpr =>
        block.caseClauses match {
          case Some(clauses) if isArgument(block) =>
            for {
              clause <- clauses.caseClauses
              body <- clause.expr
              if isMultiStatement(body)
            } addHints(block, body, sink)
          case None if isMultiStatement(block) && ScalaPsiUtil.isByNameArgument(block) =>
            addHints(block, block, sink)
          case _ =>
        }
      case _ =>
    }
  }

  private def addHints(lambda: ScExpression, body: ScExpression, sink: InlayTreeSink): Unit = {
    val results = resultExpressions(body)
    if (results.isEmpty || isUnitLambda(lambda, results)) return

    val label = labelOf(lambda)
    val pointer = SmartPointerManager.getInstance(lambda.getProject).createSmartPsiElementPointer(lambda)
    results.foreach { result =>
      sink.addPresentation(
        new InlineInlayPosition(result.endOffset, true, 0),
        null,
        null,
        HintFormat.Companion.getDefault,
        (builder: PresentationTreeBuilder) => {
          builder.text("^", null)
          builder.text(label, new InlayActionData(new PsiPointerInlayActionPayload(pointer), PsiPointerInlayActionNavigationHandler.HANDLER_ID))
          kotlin.Unit.INSTANCE
        }
      )
    }
  }

  /**
   * A body with more than one statement (`{ a; b }` or an indentation-based block),
   * or a body whose result is an `if`, `match` or `try` spanning several lines, like in Kotlin.
   */
  private def isMultiStatement(body: ScExpression): Boolean = body match {
    case block: ScBlock if !block.hasCaseClauses =>
      block.statements.sizeIs > 1 || block.resultExpression.exists(isMultiLineBranching)
    case _: ScBlock => false
    case other => isMultiLineBranching(other)
  }

  private def isMultiLineBranching(expression: ScExpression): Boolean = expression match {
    case _: ScIf | _: ScMatch | _: ScTry => expression.getText.contains('\n')
    case parenthesised: ScParenthesisedExpr => parenthesised.innerElement.exists(isMultiLineBranching)
    case _ => false
  }

  /** Expressions whose values are returned from the lambda body. */
  private def resultExpressions(expression: ScExpression): Seq[ScExpression] = expression match {
    case block: ScBlock if !block.hasCaseClauses =>
      block.resultExpression.toSeq.flatMap(resultExpressions)
    case parenthesised: ScParenthesisedExpr =>
      parenthesised.innerElement.toSeq.flatMap(resultExpressions)
    case ifExpr: ScIf if ifExpr.elseExpression.isDefined && !isOneLiner(ifExpr) =>
      (ifExpr.thenExpression.toSeq ++ ifExpr.elseExpression).flatMap(resultExpressions)
    case matchExpr: ScMatch =>
      matchExpr.clauses.flatMap(_.expr).flatMap(resultExpressions)
    case tryExpr: ScTry =>
      val catchResults = tryExpr.catchBlock.collect { case ScCatchBlock(clauses) => clauses }.toSeq
        .flatMap(_.caseClauses).flatMap(_.expr).flatMap(resultExpressions)
      tryExpr.expression.toSeq.flatMap(resultExpressions) ++ catchResults
    case _: ScReturn | _: ScThrow => Seq.empty
    case other => Seq(other)
  }

  private def isOneLiner(element: PsiElement): Boolean = !element.getText.contains('\n')

  /** The element which is shown as the lambda: `{ x => ... }` rather than the function expression inside of it. */
  private def lambdaElement(function: ScFunctionExpr): ScExpression = function.getParent match {
    case block: ScBlockExpr if block.statements == Seq(function) => block
    case _ => function
  }

  private def isArgument(expression: ScExpression): Boolean = expression.getParent match {
    case _: ScArgumentExprList => true
    case infix: ScInfixExpr => infix.right == expression
    case _ => false
  }

  /** Name of the method the lambda is passed to, e.g. `map` for `xs.map { ... }`; `lambda` otherwise. */
  private def labelOf(lambda: ScExpression): String = {
    val argument = lambda.getParent match {
      case parenthesised: ScParenthesisedExpr => parenthesised
      case _ => lambda
    }
    argument.getParent match {
      case args: ScArgumentExprList => args.callReference.map(_.refName).getOrElse(DefaultLabel)
      case infix: ScInfixExpr if infix.right == argument => infix.operation.refName
      case _ => DefaultLabel
    }
  }

  /**
   * `true` if the value of the lambda is not interesting: it is `Unit` itself (e.g. in `foreach`)
   * or it is discarded because the expected result type is `Unit`.
   */
  private def isUnitLambda(lambda: ScExpression, results: Seq[ScExpression]): Boolean = {
    implicit val context: Context = Context(lambda)
    lazy val expectsUnit = lambda match {
      case block: ScBlockExpr if !block.hasCaseClauses =>
        ScalaPsiUtil.parameterOf(block).exists(_.paramType.isUnit)
      case _ =>
        lambda.expectedType().exists {
          case FunctionType(resultType, _) => resultType.isUnit
          case PartialFunctionType(resultType, _) => resultType.isUnit
          case _ => false
        }
    }
    results.forall(_.`type`().exists(_.isUnit)) || expectsUnit
  }
}
