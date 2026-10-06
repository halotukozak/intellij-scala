package org.jetbrains.plugins.scala.codeInspection.assignment

import com.intellij.psi.{PsiElement, PsiField, PsiModifier, PsiNamedElement}
import org.jetbrains.plugins.scala.extensions.PsiNamedElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScParenthesisedExpr, ScReferenceExpression, ScThisReference}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScVariable
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject

private[assignment] object AssignmentInspectionUtil {

  /**
   * A reference to a plain variable: a local or member `var`, a `var` class parameter or a non-final Java field,
   * optionally qualified by `this`, `C.this` or a (chain of) object reference(s).
   */
  object VariableReference {
    def unapply(expression: ScExpression): Option[(ScReferenceExpression, PsiNamedElement)] = expression match {
      case ref: ScReferenceExpression if hasStableQualifier(ref) =>
        ref.resolve() match {
          case named: PsiNamedElement if isPlainVariable(named) => Some((ref, named))
          case _ => None
        }
      case _ => None
    }
  }

  private def isPlainVariable(element: PsiElement): Boolean = element match {
    case parameter: ScClassParameter => parameter.isVar
    case field: PsiField => !field.hasModifierProperty(PsiModifier.FINAL)
    case named: PsiNamedElement => named.nameContext.isInstanceOf[ScVariable]
    case _ => false
  }

  /**
   * Checks whether both expressions denote the very same variable of the very same instance.
   */
  def isSameVariable(left: ScExpression, right: ScExpression): Boolean = (unwrap(left), unwrap(right)) match {
    case (VariableReference(leftRef, leftTarget), VariableReference(rightRef, rightTarget)) =>
      leftRef.refName == rightRef.refName &&
        leftTarget == rightTarget &&
        sameQualifier(leftRef.qualifier, rightRef.qualifier)
    case _ => false
  }

  private def unwrap(expression: ScExpression): ScExpression = expression match {
    case ScParenthesisedExpr(inner) => unwrap(inner)
    case _ => expression
  }

  private def hasStableQualifier(ref: ScReferenceExpression): Boolean = ref.qualifier match {
    case None => true
    case Some(_: ScThisReference) => true
    case Some(qualifier: ScReferenceExpression) => isObjectReference(qualifier)
    case _ => false
  }

  private def isObjectReference(ref: ScReferenceExpression): Boolean =
    ref.resolve().isInstanceOf[ScObject] && (ref.qualifier match {
      case None => true
      case Some(qualifier: ScReferenceExpression) => isObjectReference(qualifier)
      case _ => false
    })

  /**
   * An unqualified reference and a `this`-qualified reference resolving to the same member denote the same variable.
   */
  private def sameQualifier(left: Option[ScExpression], right: Option[ScExpression]): Boolean = (left, right) match {
    case (None | Some(_: ScThisReference), None | Some(_: ScThisReference)) =>
      (left, right) match {
        case (Some(l: ScThisReference), Some(r: ScThisReference)) => l.refTemplate == r.refTemplate
        case _ => true
      }
    case (Some(l: ScReferenceExpression), Some(r: ScReferenceExpression)) =>
      l.resolve() == r.resolve()
    case _ => false
  }
}
