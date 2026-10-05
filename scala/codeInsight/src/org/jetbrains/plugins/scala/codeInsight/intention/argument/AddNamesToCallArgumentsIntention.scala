package org.jetbrains.plugins.scala.codeInsight.intention.argument

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiElementExt}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScArgumentExprList, ScAssignment, ScUnderscoreSection}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText

/**
 * Names all positional arguments of a call:
 * {{{
 *   foo(1, 2) -> foo(a = 1, b = 2)
 * }}}
 * Naming stops at a repeated parameter, since its arguments can't be named.
 *
 * @see [[AddNameToArgumentIntention]] which names the current and subsequent arguments
 */
final class AddNamesToCallArgumentsIntention extends PsiElementBaseIntentionAction {

  import AddNamesToCallArgumentsIntention._

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findArgumentsToName(element).isDefined

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    findArgumentsToName(element).foreach { case (list, arguments) =>
      arguments.foreach { mapped =>
        val text = NamedArgumentsUtil.nameText(mapped.param) + " = " + mapped.arg.getText
        mapped.arg.replace(createExpressionFromText(text, list)(list.getManager))
      }
    }

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.add.names.to.call.arguments")

  override def getText: String = getFamilyName
}

object AddNamesToCallArgumentsIntention {

  private def findArgumentsToName(element: PsiElement): Option[(ScArgumentExprList, Seq[NamedArgumentsUtil.MappedArgument])] = {
    val list = NamedArgumentsUtil.calleeArgumentList(element).filter(NamedArgumentsUtil.isSupported)
      .orElse(NamedArgumentsUtil.findArgumentList(element).filter(isInApplicableRange(element, _)))
      .orNull
    if (list == null) return None

    val mapped = NamedArgumentsUtil.mapArguments(list).getOrElse(return None)
    val toName = mapped
      .takeWhile(!_.param.isRepeatedParameter)
      .filterNot(_.isNamed)

    val canBeNamed = toName.forall { m =>
      !m.arg.is[ScUnderscoreSection, ScAssignment] && m.param.name.nonEmpty
    }
    if (toName.nonEmpty && canBeNamed) Some((list, toName)) else None
  }

  /**
   * Inside of the argument list, the intention is available only before the end of the first argument
   * and only if there are at least two arguments (otherwise it duplicates [[AddNameToArgumentIntention]]).
   */
  private def isInApplicableRange(element: PsiElement, list: ScArgumentExprList): Boolean =
    list.exprs match {
      case first +: _ +: _ => element.getTextRange.getEndOffset <= first.endOffset
      case _ => false
    }
}
