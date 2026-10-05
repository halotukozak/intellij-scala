package org.jetbrains.plugins.scala.codeInsight.intention.argument

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScArgumentExprList, ScAssignment, ScExpression}

/**
 * Removes the name of a named argument, moving the argument to its position if necessary:
 * {{{
 *   foo(a = 1, c = 3, b = 2) -> foo(a = 1, 2, c = 3)  // caret on `b`
 * }}}
 * Available on the name of the argument when all preceding parameters are passed in their positions.
 */
final class RemoveSingleArgumentNameIntention extends PsiElementBaseIntentionAction {

  import RemoveSingleArgumentNameIntention._

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findContext(element) match {
      case Some((_, _, name)) =>
        setText(ScalaCodeInsightBundle.message("remove.argument.name.0", name))
        true
      case None => false
    }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    findContext(element).foreach { case (list, newArguments, _) =>
      NamedArgumentsUtil.rewriteArguments(list, newArguments)
    }

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.remove.argument.name")
}

object RemoveSingleArgumentNameIntention {

  private def findContext(element: PsiElement): Option[(ScArgumentExprList, Seq[(ScExpression, Boolean)], String)] = {
    val assignment = PsiTreeUtil.getParentOfType(element, classOf[ScAssignment], false)
    if (assignment == null) return None

    val list = assignment.getContext match {
      case list: ScArgumentExprList if NamedArgumentsUtil.isSupported(list) => list
      case _ => return None
    }

    // available only on the name and `=`
    val value = NamedArgumentsUtil.namedArgumentValue(assignment).getOrElse(return None)
    if (element.startOffset >= value.startOffset) return None

    val mapped = NamedArgumentsUtil.mapArguments(list).getOrElse(return None)
    val current = mapped.find(_.arg == assignment).filter(_.isNamed).getOrElse(return None)

    val positional = NamedArgumentsUtil.argumentsThatCanBeUnnamed(mapped)
    val position = positional.indexOf(current)
    if (position < 0) return None

    // all preceding arguments must already be in their positions
    val actualPositions = mapped.map(_.arg).zipWithIndex.toMap
    val precedingInPlace = positional.take(position).zipWithIndex.forall {
      case (m, index) => actualPositions(m.arg) == index
    }
    if (!precedingInPlace) return None

    val others = mapped.filterNot(_ == current).map(m => (m.arg, false))
    val (before, after) = others.splitAt(position)
    val newArguments = (before :+ ((current.arg, true))) ++ after
    Some((list, newArguments, current.param.name))
  }
}
