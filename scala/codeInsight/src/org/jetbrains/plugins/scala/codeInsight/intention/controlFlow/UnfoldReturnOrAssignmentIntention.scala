package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText

/**
 * Converts {{{
 *   return if (c) a else b
 *   x = if (c) a else b
 * }}} to {{{
 *   if (c) return a else return b
 *   if (c) x = a else x = b
 * }}}
 * Works for `if`/`else`, `match` and `try`/`catch`, including nested `if`/`else if` chains and multi-statement branches.
 * Value definitions (`val x = if ...`, `var x = if ...`) are not assignments and are not supported.
 *
 * Kotlin plugin analogues: `UnfoldReturnToIfIntention`, `UnfoldReturnToWhenIntention`,
 * `UnfoldAssignmentToIfIntention`, `UnfoldAssignmentToWhenIntention`
 */
final class UnfoldReturnOrAssignmentIntention extends PsiElementBaseIntentionAction with DumbAware {

  import UnfoldReturnOrAssignmentIntention._

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findUnfoldable(element) match {
      case Some(Unfoldable(statement, branching, _, _)) =>
        val keyword = BranchTails.keywordText(branching)
        val text = statement match {
          case _: ScReturn => ScalaCodeInsightBundle.message("replace.return.with.expression", keyword)
          case _ => ScalaCodeInsightBundle.message("replace.assignment.with.expression", keyword)
        }
        setText(text)
        true
      case None => false
    }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit = {
    val Unfoldable(statement, branching, prefix, tails) = findUnfoldable(element).getOrElse(return)

    IntentionPreviewUtils.write { () =>
      tails.filterNot(BranchTails.isJump).foreach { tail =>
        val unfolded = createExpressionFromText(prefix + tail.getText, tail)(tail.projectContext)
        tail.replaceExpression(unfolded, removeParenthesis = true)
      }
      statement.replaceExpression(branching, removeParenthesis = true)
    }
  }

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.unfold.return.or.assignment")
}

object UnfoldReturnOrAssignmentIntention {

  private final case class Unfoldable(statement: ScExpression,
                                      branching: ScExpression,
                                      prefix: String,
                                      tails: Seq[ScExpression])

  private def findUnfoldable(element: PsiElement): Option[Unfoldable] =
    element.withParentsInFile
      .takeWhile(e => !e.is[ScBlock])
      .collectFirst {
        case ret: ScReturn => ret
        case assignment: ScAssignment => assignment
      }
      .flatMap(unfoldable(_, element))

  private def unfoldable(statement: ScExpression, element: PsiElement): Option[Unfoldable] = {    val (valueOpt, prefix) = statement match {
      case ret: ScReturn =>
        (ret.expr, "return ")
      case assignment: ScAssignment if isUnfoldableAssignment(assignment) =>
        (assignment.rightExpression, s"${assignment.leftExpression.getText} = ")
      case _ =>
        (None, "")
    }

    for {
      value <- valueOpt
      // the caret should be on the `return` keyword, on the assignment target or on `=`
      if element.getTextRange.getEndOffset <= value.getTextRange.getStartOffset
      if BranchTails.isBranching(value)
      tails <- BranchTails.collect(value)
      if !tails.forall(BranchTails.isJump)
    } yield Unfoldable(statement, value, prefix, tails)
  }

  private def isUnfoldableAssignment(assignment: ScAssignment): Boolean =
    !isPossiblyNamedArgument(assignment) && BranchTails.isSimpleAssignmentTarget(assignment.leftExpression)

  /** Syntactic check (the intention is dumb-aware): `foo(x = ...)`, `(x = ...)` or `a op (x = ...)` */
  private def isPossiblyNamedArgument(assignment: ScAssignment): Boolean =
    assignment.getParent.is[ScArgumentExprList, ScTuple, ScParenthesisedExpr, ScInfixExpr]
}
