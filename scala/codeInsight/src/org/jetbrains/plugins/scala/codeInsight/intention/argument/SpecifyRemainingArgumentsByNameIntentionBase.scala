package org.jetbrains.plugins.scala.codeInsight.intention.argument

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.codeInsight.template.TemplateBuilderImpl
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScArgumentExprList, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter

/**
 * Adds named arguments with `???` placeholders for the parameters, which don't have arguments yet:
 * {{{
 *   def foo(a: Int, b: Int = 0, c: Int)
 *   foo() -> foo(a = ???, c = ???)          // required
 *   foo() -> foo(a = ???, b = ???, c = ???) // all
 * }}}
 * The placeholders are edited with a live template.
 *
 * Available on the name of the invoked method or on the argument list outside of the arguments.
 * Repeated parameters are skipped, since their arguments can't be named.
 */
sealed abstract class SpecifyRemainingArgumentsByNameIntentionBase extends PsiElementBaseIntentionAction {

  import SpecifyRemainingArgumentsByNameIntentionBase._

  protected def parametersToAdd(remaining: RemainingParameters): Option[Seq[ScParameter]]

  override def getText: String = getFamilyName

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findContextFor(this, element).isDefined

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit = {
    val (list, parameters) = findContextFor(this, element).getOrElse(return)

    val document = editor.getDocument
    val documentManager = PsiDocumentManager.getInstance(project)
    documentManager.doPostponedOperationsAndUnblockDocument(document)

    val lastComma = Option(list.getLastChild)
      .flatMap(_.prevSiblings.find(_.elementType == ScalaTokenTypes.tCOMMA))
      .filter(_ => list.missedLastExpr)
    val (offset, prefix) = (list.exprs.lastOption, lastComma) match {
      case (_, Some(comma))  => (comma.endOffset, " ")
      case (Some(last), None) => (last.endOffset, ", ")
      case (None, None) =>
        val leftParen = list.getFirstChild
        (leftParen.endOffset, "")
    }

    val newArguments = parameters.map(p => NamedArgumentsUtil.nameText(p) + " = " + Placeholder)
    document.insertString(offset, prefix + newArguments.mkString(", "))
    documentManager.commitDocument(document)

    val placeholderOffsets = newArguments.scanLeft(offset + prefix.length) {
      case (start, argument) => start + argument.length + 2
    }.init.zip(newArguments).map {
      case (start, argument) => start + argument.length - Placeholder.length
    }

    if (!IntentionPreviewUtils.isIntentionPreviewActive)
      startTemplate(project, editor, placeholderOffsets)
    else
      editor.getCaretModel.moveToOffset(placeholderOffsets.head)
  }

  private def startTemplate(project: Project, editor: Editor, placeholderOffsets: Seq[Int]): Unit = {
    val file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument)
    if (file == null) return

    val placeholders = placeholderOffsets.flatMap { offset =>
      Option(PsiTreeUtil.findElementOfClassAtOffset(file, offset, classOf[ScReferenceExpression], true))
    }
    val newList = placeholders.headOption.flatMap(p => Option(PsiTreeUtil.getParentOfType(p, classOf[ScArgumentExprList])))
    newList match {
      case Some(list) if placeholders.size == placeholderOffsets.size =>
        val builder = new TemplateBuilderImpl(list)
        placeholders.foreach(builder.replaceElement(_, Placeholder))
        builder.run(editor, true)
      case _ =>
        editor.getCaretModel.moveToOffset(placeholderOffsets.head)
    }
  }
}

object SpecifyRemainingArgumentsByNameIntentionBase {

  private val Placeholder = "???"

  final case class RemainingParameters(required: Seq[ScParameter], all: Seq[ScParameter])

  private def findContext(element: PsiElement): Option[(ScArgumentExprList, RemainingParameters)] = {
    val list = NamedArgumentsUtil.findArgumentList(element).getOrElse(return None)
    if (!isInApplicableRange(element, list)) return None

    val clause = NamedArgumentsUtil.parameterClause(list).getOrElse(return None)
    val parameters = clause.parameters

    val used = list.exprs.zipWithIndex.map { case (arg, position) =>
      NamedArgumentsUtil.argumentName(arg).filter(name => parameters.exists(_.name == name)) match {
        case Some(name) => name
        case None =>
          parameters.lift(position) match {
            case Some(param) if !param.isRepeatedParameter => param.name
            // arguments after a repeated parameter or excess arguments
            case _ => return None
          }
      }
    }.toSet

    val all = parameters.filter(p => !used.contains(p.name) && !p.isRepeatedParameter && p.name.nonEmpty)
    val required = all.filterNot(_.isDefaultParam)
    if (required.isEmpty) None
    else Some((list, RemainingParameters(required, all)))
  }

  /** On the callee, or inside of the argument list before the first argument / after the last one. */
  private def isInApplicableRange(element: PsiElement, list: ScArgumentExprList): Boolean =
    NamedArgumentsUtil.calleeArgumentList(element).contains(list) || {
      val exprs = list.exprs
      exprs.isEmpty ||
        element.endOffset <= exprs.head.startOffset ||
        element.startOffset >= exprs.last.endOffset
    }

  private def findContextFor(intention: SpecifyRemainingArgumentsByNameIntentionBase,
                             element: PsiElement): Option[(ScArgumentExprList, Seq[ScParameter])] =
    for {
      (list, remaining) <- findContext(element)
      parameters <- intention.parametersToAdd(remaining)
    } yield (list, parameters)
}

/**
 * Specifies all the remaining required arguments by name. Available if there are also parameters with default values,
 * otherwise [[SpecifyAllRemainingArgumentsByNameIntention]] does the same.
 */
final class SpecifyRemainingRequiredArgumentsByNameIntention extends SpecifyRemainingArgumentsByNameIntentionBase {
  override protected def parametersToAdd(remaining: SpecifyRemainingArgumentsByNameIntentionBase.RemainingParameters): Option[Seq[ScParameter]] =
    Option.when(remaining.required.nonEmpty && remaining.required != remaining.all)(remaining.required)

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.specify.remaining.required.arguments.by.name")
}

/**
 * Specifies all the remaining arguments by name, including the parameters with default values.
 */
final class SpecifyAllRemainingArgumentsByNameIntention extends SpecifyRemainingArgumentsByNameIntentionBase {
  override protected def parametersToAdd(remaining: SpecifyRemainingArgumentsByNameIntentionBase.RemainingParameters): Option[Seq[ScParameter]] =
    Option.when(remaining.all.nonEmpty)(remaining.all)

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.specify.all.remaining.arguments.by.name")
}
