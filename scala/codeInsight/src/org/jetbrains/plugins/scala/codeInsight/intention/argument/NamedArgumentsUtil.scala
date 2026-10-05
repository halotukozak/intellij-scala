package org.jetbrains.plugins.scala.codeInsight.intention.argument

import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScConstructorInvocation, ScMethodLike}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScParameterClause}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

import scala.annotation.tailrec

/**
 * Shared logic of the intentions working with named arguments:
 *  - [[AddNamesToCallArgumentsIntention]]
 *  - [[RemoveAllArgumentNamesIntention]]
 *  - [[RemoveSingleArgumentNameIntention]]
 *  - [[SpecifyRemainingArgumentsByNameIntentionBase]]
 *
 * Only argument lists in parentheses are supported (no block arguments).
 * `using` argument lists are not supported.
 */
private[argument] object NamedArgumentsUtil {

  /** An argument (a direct child of the argument list) together with the parameter it is passed to. */
  final case class MappedArgument(arg: ScExpression, param: ScParameter) {
    def isNamed: Boolean = argumentName(arg).contains(param.name)

    /** Index of the parameter inside its own parameter clause. */
    def paramIndex: Int = parameterIndex(param)
  }

  /**
   * Finds the argument list of the call the element belongs to:
   *  - the caret is on the name of the invoked method/constructor, or
   *  - the caret is inside of an argument list.
   */
  def findArgumentList(element: PsiElement): Option[ScArgumentExprList] =
    calleeArgumentList(element)
      .orElse(enclosingArgumentList(element))
      .filter(isSupported)

  def isSupported(list: ScArgumentExprList): Boolean =
    list.isArgsInParens && !list.isUsing && (list.getContext match {
      case _: ScMethodCall | _: ScConstructorInvocation => true
      case _ => false
    })

  def calleeArgumentList(element: PsiElement): Option[ScArgumentExprList] = {
    def argsOf(invoked: ScExpression): Option[ScArgumentExprList] = invoked.getContext match {
      case call: ScMethodCall if call.getInvokedExpr == invoked => Some(call.args)
      case gen: ScGenericCall if gen.referencedExpr == invoked => argsOf(gen)
      case _ => None
    }

    element.getContext match {
      case ref: ScReferenceExpression if ref.nameId == element => argsOf(ref)
      case _ =>
        val constrInvocation = PsiTreeUtil.getContextOfType(element, classOf[ScConstructorInvocation])
        if (constrInvocation != null && constrInvocation.typeElement.isAncestorOf(element)) constrInvocation.args
        else None
    }
  }

  private def enclosingArgumentList(element: PsiElement): Option[ScArgumentExprList] =
    element.withParentsInFile
      .takeWhile(e => !e.is[ScBlock] && !e.is[ScTemplateBody])
      .collectFirst { case list: ScArgumentExprList => list }

  /** @return the argument (direct child of `list`) which contains `expr` */
  @tailrec
  def directArgument(list: ScArgumentExprList, expr: PsiElement): Option[ScExpression] =
    if (expr == null || expr == list) None
    else expr.getContext match {
      case `list` => expr.asOptionOf[ScExpression]
      case context => directArgument(list, context)
    }

  /** Name of a named argument `name = expr`, if the argument has the form of a named argument. */
  def argumentName(arg: ScExpression): Option[String] = arg match {
    case ScAssignment(ref: ScReferenceExpression, Some(_)) if ref.qualifier.isEmpty => Some(ref.refName)
    case _ => None
  }

  def namedArgumentValue(arg: ScExpression): Option[ScExpression] = arg match {
    case assignment: ScAssignment if argumentName(arg).isDefined => assignment.rightExpression
    case _ => None
  }

  def parameterIndex(param: ScParameter): Int = param.getParent match {
    case clause: ScParameterClause => clause.parameters.indexOf(param)
    case _ => -1
  }

  def nameText(param: ScParameter): String = ScalaNamesUtil.escapeKeyword(param.name)

  /**
   * Maps every argument of the list to a Scala parameter.
   *
   * @return None if some argument can't be mapped (e.g. red code, java method),
   *         or if the parameters don't belong to a single parameter clause
   */
  def mapArguments(list: ScArgumentExprList): Option[Seq[MappedArgument]] = {
    val exprs = list.exprs
    if (exprs.isEmpty) return None

    val byArgument = list.matchedParameters
      .filter { case (expr, _) => list.isAncestorOf(expr) }
      .flatMap { case (expr, param) =>
        for {
          arg <- directArgument(list, expr)
          scParam <- param.psiParam.collect { case p: ScParameter => p }
        } yield arg -> scParam
      }
      .groupBy(_._1)

    val mapped = exprs.map { arg =>
      byArgument.get(arg) match {
        case Some(Seq((_, param))) => MappedArgument(arg, param)
        case _ => return None
      }
    }

    val clauses = mapped.map(_.param.getParent).distinct
    if (clauses.size == 1 && clauses.head.is[ScParameterClause]) Some(mapped)
    else None
  }

  /**
   * Arguments which can be passed positionally, sorted in the order of their parameters.
   * It's the longest prefix of arguments (sorted by parameter positions) without gaps.
   */
  def argumentsThatCanBeUnnamed(mapped: Seq[MappedArgument]): Seq[MappedArgument] = {
    val sorted = mapped.sortBy(m => (m.paramIndex, m.arg.getTextRange.getStartOffset))
    sorted.zipWithIndex.takeWhile { case (m, position) =>
      val index = m.paramIndex
      index == position || (m.param.isRepeatedParameter && index < position && !m.isNamed)
    }.map(_._1)
  }

  /**
   * Rewrites the argument list so that it consists of `newArguments` (in this order).
   * Arguments with the flag set to `true` lose their names.
   */
  def rewriteArguments(list: ScArgumentExprList, newArguments: Seq[(ScExpression, Boolean)]): Unit = {
    val oldArguments = list.exprs
    if (oldArguments.size != newArguments.size) return

    val replacements = oldArguments.zip(newArguments).collect {
      case (old, (arg, unname)) if old != arg || unname =>
        val source = if (unname) namedArgumentValue(arg).getOrElse(arg) else arg
        old -> source.copy()
    }
    replacements.foreach { case (old, replacement) => old.replace(replacement) }
  }

  /**
   * Finds the parameter clause, which corresponds to the argument list.
   * Handles multiple argument lists, `using` clauses (they are skipped unless passed explicitly),
   * `apply` calls and constructor invocations.
   */
  def parameterClause(list: ScArgumentExprList): Option[ScParameterClause] = list.getContext match {
    case call: ScMethodCall =>
      @tailrec
      def resolvedCall(call: ScMethodCall, lists: List[ScArgumentExprList]): Option[(PsiElement, List[ScArgumentExprList])] =
        call.target match {
          case Some(target) => Some((target.element, call.args :: lists))
          case None =>
            call.getEffectiveInvokedExpr match {
              case inner: ScMethodCall => resolvedCall(inner, call.args :: lists)
              case _ => None
            }
        }

      for {
        (target, lists) <- resolvedCall(call, Nil)
        method <- target.asOptionOf[ScMethodLike]
        clause <- clauseFor(method.parameterList.clauses.toList, lists)
      } yield clause
    case constrInvocation: ScConstructorInvocation =>
      val target = constrInvocation.reference.flatMap { ref =>
        ref.bind().orElse(ref.resolveAllConstructors match {
          case Array(single) => Some(single)
          case _ => None
        })
      }
      for {
        srr <- target
        constructor <- srr.element.asOptionOf[ScMethodLike]
        lists = constrInvocation.arguments.takeWhile(_ != list) :+ list
        clause <- clauseFor(constructor.parameterList.clauses.toList, lists.toList)
      } yield clause
    case _ => None
  }

  /** @param lists argument lists of the call, the last one is the one we are looking the clause for */
  @tailrec
  private def clauseFor(clauses: List[ScParameterClause], lists: List[ScArgumentExprList]): Option[ScParameterClause] =
    lists match {
      case Nil => None
      case list :: rest =>
        val candidates =
          if (list.isUsing) clauses.dropWhile(!_.hasUsingKeyword)
          else clauses.dropWhile(_.hasUsingKeyword)
        candidates match {
          case clause :: _ if rest.isEmpty => Some(clause)
          case _ :: others => clauseFor(others, rest)
          case Nil => None
        }
    }
}
