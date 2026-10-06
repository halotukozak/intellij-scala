package org.jetbrains.plugins.scala.codeInspection.controlFlow

import com.intellij.codeInspection.options.OptPane
import com.intellij.codeInspection.options.OptPane.{checkbox, pane}
import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.psi.{PsiElement, PsiElementVisitor}
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.codeInsight.intention.controlFlow.BranchTails
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText

import scala.beans.BooleanBeanProperty

/**
 * Reports `if`/`else`, `match` and `try`/`catch` statements in which every branch ends with a `return`,
 * or with an assignment to the same variable, and offers to lift the `return` or the assignment out: {{{
 *   if (c) return a else return b   ->   return if (c) a else b
 *   if (c) x = a else x = b         ->   x = if (c) a else b
 * }}}
 * Nested `if`/`else if` chains, nested branching expressions and multi-statement branches are supported.
 * Branches ending with `throw` are left untouched.
 *
 * Mirrors Kotlin's `LiftReturnOrAssignmentInspection`: only the keyword is highlighted, and the problem is reported
 * with the configured severity only when lifting is clearly an improvement. Otherwise (a single liftable branch,
 * multi-statement branches, other `return`s inside, a long expression), it is reported as INFORMATION,
 * i.e. the quick fix is still available via Alt+Enter but nothing is highlighted.
 *
 * The reverse conversion is [[org.jetbrains.plugins.scala.codeInsight.intention.controlFlow.UnfoldReturnOrAssignmentIntention]].
 */
final class ScalaLiftReturnOrAssignmentInspection extends LocalInspectionTool with DumbAware {

  import ScalaLiftReturnOrAssignmentInspection._

  @BooleanBeanProperty
  var reportOnlyIfSingleStatement: Boolean = true

  override def getOptionsPane: OptPane = pane(
    checkbox(
      "reportOnlyIfSingleStatement",
      ScalaCodeInsightBundle.message("lift.return.or.assignment.option.only.single.statement")
    )
  )

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = PsiElementVisitorSimple(holder) {
    case expr: ScExpression if BranchTails.isBranching(expr) && !isElseIfBranch(expr) =>
      for {
        liftable <- findLiftable(expr)
        if !isNestedInLiftable(expr, liftable)
        keyword <- keywordElement(expr)
      } {
        val highlightType =
          if (isSerious(expr, liftable)) ProblemHighlightType.GENERIC_ERROR_OR_WARNING
          else ProblemHighlightType.INFORMATION

        if (isOnTheFly || highlightType != ProblemHighlightType.INFORMATION) {
          val keywordText = keyword.getText
          val message = liftable.kind match {
            case Return => ScalaCodeInsightBundle.message("return.can.be.lifted.out.of", keywordText)
            case _: Assignment => ScalaCodeInsightBundle.message("assignment.can.be.lifted.out.of", keywordText)
          }
          holder.registerProblem(
            expr,
            message,
            highlightType,
            keyword.getTextRange.shiftLeft(expr.getTextRange.getStartOffset),
            new LiftReturnOrAssignmentQuickFix(expr, liftable.kind, keywordText)
          )
        }
      }
    case _ =>
  }

  private def isSerious(expr: ScExpression, liftable: Liftable): Boolean = {
    val lifted = liftable.tails.filterNot(_.is[ScThrow])
    val hasOtherReturns = liftable.kind == Return && expr.depthFirst().exists {
      case ret: ScReturn => !lifted.contains(ret)
      case _ => false
    }
    lineCount(expr) <= LinesLimit &&
      lifted.size > 1 &&
      !hasOtherReturns &&
      (!reportOnlyIfSingleStatement || lifted.forall(isSingleStatement))
  }
}

object ScalaLiftReturnOrAssignmentInspection {

  /** Expressions longer than this are reported only as INFORMATION (Kotlin doesn't report them at all) */
  private val LinesLimit = 15

  private sealed trait Kind
  private case object Return extends Kind
  private final case class Assignment(target: String) extends Kind

  private final case class Liftable(kind: Kind, tails: Seq[ScExpression])

  private final class LiftReturnOrAssignmentQuickFix(expr: ScExpression, kind: Kind, keyword: String)
    extends AbstractFixOnPsiElement(
      kind match {
        case Return => ScalaCodeInsightBundle.message("lift.return.out.of", keyword)
        case _: Assignment => ScalaCodeInsightBundle.message("lift.assignment.out.of", keyword)
      },
      expr
    ) {

    override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.lift.return.or.assignment")

    override protected def doApplyFix(expr: ScExpression)(implicit project: Project): Unit =
      findLiftable(expr).foreach(lift(expr, _))
  }

  private def lift(expr: ScExpression, liftable: Liftable): Unit = {
    liftable.tails.foreach {
      case ret@ScReturn(value) => ret.replaceExpression(value, removeParenthesis = true)
      case assignment: ScAssignment => assignment.rightExpression.foreach(assignment.replaceExpression(_, removeParenthesis = true))
      case _ =>
    }

    val prefix = liftable.kind match {
      case Return => "return "
      case Assignment(target) => s"$target = "
    }
    val lifted = createExpressionFromText(prefix + expr.getText, expr)(expr.projectContext)
    expr.replaceExpression(lifted, removeParenthesis = true)
  }

  private def findLiftable(expr: ScExpression): Option[Liftable] =
    if (BranchTails.isBranching(expr) && isStatementPosition(expr))
      for {
        tails <- BranchTails.collect(expr)
        kind <- liftKind(tails)
      } yield Liftable(kind, tails)
    else None

  private def keywordElement(expr: ScExpression): Option[PsiElement] = expr match {
    case _: ScIf => expr.findFirstChildByType(ScalaTokenTypes.kIF)
    case _: ScMatch => expr.findFirstChildByType(ScalaTokenTypes.kMATCH)
    case _: ScTry => expr.findFirstChildByType(ScalaTokenTypes.kTRY)
    case _ => None
  }

  /** For `if (a) ... else if (b) ... else ...` only the whole chain is reported */
  private def isElseIfBranch(expr: ScExpression): Boolean = expr.getParent match {
    case parentIf: ScIf => parentIf.elseExpression.contains(expr)
    case _ => false
  }

  /** A nested branching expression is not reported when an enclosing one lifts the same tails */
  private def isNestedInLiftable(expr: ScExpression, liftable: Liftable): Boolean =
    expr.parentsInFile.exists {
      case outer: ScExpression if BranchTails.isBranching(outer) =>
        findLiftable(outer).exists(outerLiftable => liftable.tails.forall(outerLiftable.tails.contains))
      case _ => false
    }

  private def isStatementPosition(expr: ScExpression): Boolean =
    expr.getParent.is[ScBlock, ScFunctionDefinition, ScTemplateBody, ScalaFile]

  private def isSingleStatement(tail: ScExpression): Boolean = tail.getParent match {
    case block: ScBlock => block.statements.size <= 1
    case _ => true
  }

  private def lineCount(expr: ScExpression): Int = expr.getText.count(_ == '\n') + 1

  private def liftKind(tails: Seq[ScExpression]): Option[Kind] = {
    val nonThrowing = tails.filterNot(_.is[ScThrow])
    nonThrowing.headOption.flatMap {
      case ScReturn(_) if nonThrowing.forall(_.is[ScReturn]) && nonThrowing.forall(_.asInstanceOf[ScReturn].expr.isDefined) =>
        Some(Return)
      case first: ScAssignment if isLiftableAssignment(first) =>
        val target = first.leftExpression.getText
        val allSameTarget = nonThrowing.forall {
          case assignment: ScAssignment => isLiftableAssignment(assignment) && assignment.leftExpression.getText == target
          case _ => false
        }
        Option.when(allSameTarget)(Assignment(target))
      case _ => None
    }
  }

  private def isLiftableAssignment(assignment: ScAssignment): Boolean =
    assignment.rightExpression.isDefined && BranchTails.isSimpleAssignmentTarget(assignment.leftExpression)
}
