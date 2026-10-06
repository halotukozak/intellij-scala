package org.jetbrains.plugins.scala.codeInspection.assignment

import com.intellij.codeInspection.options.OptPane
import com.intellij.codeInspection.options.OptPane.{checkbox, pane}
import com.intellij.codeInspection.{LocalInspectionTool, LocalQuickFix, ProblemsHolder}
import com.intellij.modcommand.{ActionContext, ModPsiUpdater, PsiUpdateModCommandAction}
import com.intellij.psi.PsiElementVisitor
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.assignment.AssignmentInspectionUtil.isSameVariable
import org.jetbrains.plugins.scala.codeInspection.{PsiElementVisitorSimple, ScalaInspectionBundle, conformsToTypeFromClass}
import org.jetbrains.plugins.scala.extensions.ObjectExt
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.types.Context
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil

import scala.beans.BooleanBeanProperty

/**
 * Reports reassignments which can be simplified with an assignment operator:
 *  - `x = x op e` with `x op= e`,
 *  - `x = e op: x` with `x op:= e` (right-associative operators, optional),
 *  - `map = map.updated(k, v)` with `map += k -> v` (immutable maps).
 *
 * The inspection is reported only when `x op= e` would desugar into exactly the same reassignment,
 * i.e. when the type of `x` has no `op=` method of its own (as mutable collections do).
 */
final class ScalaOperatorAssignmentInspection extends LocalInspectionTool {

  import ScalaOperatorAssignmentInspection._

  @BooleanBeanProperty
  var reportRightAssociative: Boolean = true

  override def getOptionsPane: OptPane = pane(
    checkbox("reportRightAssociative", ScalaInspectionBundle.message("simplify.reassignment.report.right.associative"))
  )

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitor = PsiElementVisitorSimple(holder) {
    case assignment: ScAssignment if !isArgument(assignment) =>
      for {
        replacement <- operatorAssignmentText(assignment, reportRightAssociative)
      } holder.registerProblem(
        assignment,
        ScalaInspectionBundle.message("reassignment.can.be.simplified"),
        LocalQuickFix.from(new ReplaceWithOperatorAssignmentQuickFix(assignment, replacement))
      )
    case _ =>
  }
}

object ScalaOperatorAssignmentInspection {

  private def isArgument(assignment: ScAssignment): Boolean =
    assignment.getParent match {
      case _: ScArgumentExprList | _: ScTuple => true
      case _ => assignment.isNamedParameter
    }

  private def isSymbolicOperator(name: String): Boolean =
    name.nonEmpty && name.forall(ScalaNamesUtil.isOpCharacter)

  private[assignment] def operatorAssignmentText(assignment: ScAssignment, reportRightAssociative: Boolean): Option[String] = {
    val left = assignment.leftExpression
    val candidate = assignment.rightExpression.flatMap {
      // `x = x op e` or, for a right-associative `op`, `x = e op x`
      case infix@ScInfixExpr.withAssoc(base, operation, argument)
        if infix.typeArgs.isEmpty &&
          (reportRightAssociative || !infix.isRightAssoc) &&
          isSymbolicOperator(operation.refName) &&
          ParserUtils.isAssignmentOperator(operation.refName + "=") &&
          isSameVariable(left, base) =>
        Some(s"${left.getText} ${operation.refName}= ${argument.getText}")
      // `map = map.updated(k, v)`
      case ScMethodCall(ref@ScReferenceExpression.withQualifier(qualifier), Seq(key, value))
        if ref.refName == "updated" &&
          !key.is[ScAssignment] && !value.is[ScAssignment] &&
          isSameVariable(left, qualifier) &&
          isImmutableMap(left) =>
        Some(s"${left.getText} += ${tupleElementText(key)} -> ${tupleElementText(value)}")
      case _ => None
    }

    candidate.filter { text =>
      ScalaPsiElementFactory.createExpressionWithContextFromText(text, assignment.getContext, assignment) match {
        // `isAssignment` checks that `x op= e` is resolved as `x = x op e` and not as a call of an `op=` method
        case newInfix: ScInfixExpr => newInfix.isAssignment
        case _ => false
      }
    }
  }

  private def isImmutableMap(expression: ScExpression): Boolean =
    expression.`type`().toOption.exists { tpe =>
      conformsToTypeFromClass(tpe, "scala.collection.immutable.Map")(tpe.projectContext, Context(expression))
    }

  /**
   * An operand of `->` which binds tighter than `->` itself is left as is, anything else is parenthesized.
   */
  private def tupleElementText(expression: ScExpression): String = expression match {
    case _: ScReferenceExpression | _: ScLiteral | _: ScMethodCall | _: ScGenericCall |
         _: ScParenthesisedExpr | _: ScTuple | _: ScThisReference | _: ScPrefixExpr => expression.getText
    case _ => s"(${expression.getText})"
  }

  private final class ReplaceWithOperatorAssignmentQuickFix(assignment: ScAssignment, @Nls replacement: String)
    extends PsiUpdateModCommandAction[ScAssignment](assignment) {

    override def getFamilyName: String = ScalaInspectionBundle.message("replace.with.operator.assignment", replacement)

    override def invoke(context: ActionContext, assignment: ScAssignment, updater: ModPsiUpdater): Unit =
      assignment.replaceExpression(ScalaPsiElementFactory.createExpressionFromText(replacement, assignment)(assignment.projectContext), removeParenthesis = false)
  }
}
