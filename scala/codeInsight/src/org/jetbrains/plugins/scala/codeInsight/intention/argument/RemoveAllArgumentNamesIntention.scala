package org.jetbrains.plugins.scala.codeInsight.intention.argument

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.codeInsight.intention.argument.NamedArgumentsUtil.MappedArgument
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScArgumentExprList, ScExpression}

/**
 * Removes names of the arguments of a call, reordering them to match the parameter order:
 * {{{
 *   foo(b = 2, a = 1) -> foo(1, 2)
 * }}}
 * If some arguments can't be passed positionally (e.g. a parameter with a default value is skipped),
 * they keep their names:
 * {{{
 *   def foo(a: Int, b: Int = 0, c: Int, d: Int)
 *   foo(d = 4, a = 1, c = 3) -> foo(1, d = 4, c = 3)
 * }}}
 */
final class RemoveAllArgumentNamesIntention extends PsiElementBaseIntentionAction {

  import RemoveAllArgumentNamesIntention._

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findContext(element) match {
      case Some(context) =>
        setText(
          if (context.removesAll) getFamilyName
          else ScalaCodeInsightBundle.message("remove.all.possible.argument.names")
        )
        true
      case None => false
    }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    findContext(element).foreach { context =>
      NamedArgumentsUtil.rewriteArguments(context.list, context.newArguments)
    }

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.remove.all.argument.names")
}

object RemoveAllArgumentNamesIntention {

  private final case class Context(list: ScArgumentExprList,
                                   newArguments: Seq[(ScExpression, Boolean)],
                                   removesAll: Boolean)

  private def findContext(element: PsiElement): Option[Context] = for {
    list <- NamedArgumentsUtil.findArgumentList(element)
    mapped <- NamedArgumentsUtil.mapArguments(list)
    context <- createContext(list, mapped)
  } yield context

  private def createContext(list: ScArgumentExprList, mapped: Seq[MappedArgument]): Option[Context] = {
    val positional = NamedArgumentsUtil.argumentsThatCanBeUnnamed(mapped)

    // the case of a single name is covered by RemoveSingleArgumentNameIntention
    if (positional.count(_.isNamed) < 2) return None

    val positionalArgs = positional.map(_.arg).toSet
    val rest = mapped.filterNot(m => positionalArgs.contains(m.arg))
    // positional arguments, which can't be moved to their positions: red code
    if (rest.exists(!_.isNamed)) return None

    val newArguments = positional.map(m => (m.arg, m.isNamed)) ++ rest.map(m => (m.arg, false))
    Some(Context(list, newArguments, removesAll = rest.isEmpty))
  }
}
