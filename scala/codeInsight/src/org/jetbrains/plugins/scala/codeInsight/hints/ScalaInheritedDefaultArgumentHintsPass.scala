package org.jetbrains.plugins.scala.codeInsight.hints

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.util.text.StringUtil
import com.intellij.pom.Navigatable
import com.intellij.psi.PsiFile
import org.jetbrains.plugins.scala.annotator.hints.Hint.HintPosition
import org.jetbrains.plugins.scala.annotator.hints.{Hint, Text}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.incremental.Highlighting.ElementHighlightingExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.settings.ScalaApplicationSettings.{getInstance => ScalaApplicationSettings}

/**
 * X-Ray hints for inherited default arguments, SCL-25813.
 *
 * A method that overrides or implements a super method inherits the default arguments of that method,
 * which is easy to overlook, so in X-Ray mode we show them on the parameters of the overriding method:
 * {{{
 *   class Foo:
 *     def foo(x: Int = 0): Unit
 *
 *   class Bar extends Foo:
 *     def foo(x: Int/*< = 0>*/): Unit = {}
 * }}}
 *
 * The default expression navigates to its declaration in the super method.
 */
private[codeInsight] trait ScalaInheritedDefaultArgumentHintsPass {

  protected def collectInheritedDefaultArgumentHints(editor: Editor, root: PsiFile): Seq[Hint] =
    if (!(ScalaHintsSettings.xRayMode && ScalaApplicationSettings.XRAY_SHOW_INHERITED_DEFAULT_ARGUMENT_HINTS)) Seq.empty
    else
      root.elements(_.isVisible(editor.getProject, root)).flatMap {
        case parameter: ScParameter => ScalaInheritedDefaultArgumentHintsPass.hintFor(parameter)
        case _ => None
      }.toSeq
}

private object ScalaInheritedDefaultArgumentHintsPass {

  private def hintFor(parameter: ScParameter): Option[Hint] =
    if (!parameter.owner.is[ScFunction] || parameter.getActualDefaultExpression.isDefined) None
    else
      parameter.getSuperParameter.flatMap(_.getDefaultExpression).map { expression =>
        val text = ScalaDefaultArgumentHintsPass.presentableText(expression)
        val fullText = expression.getText.replaceAll("\\s+", " ").trim
        val tooltip = () => Option.when(text != fullText)(StringUtil.escapeXmlEntities(fullText))

        Hint(
          Seq(
            Text(s" ${ScalaTokenTypes.tASSIGN} "),
            Text(text, navigatable = expression.asOptionOfUnsafe[Navigatable], tooltip = tooltip)
          ),
          parameter,
          position = HintPosition.AfterElement,
          relatesToPrecedingElement = true
        )
      }
}
