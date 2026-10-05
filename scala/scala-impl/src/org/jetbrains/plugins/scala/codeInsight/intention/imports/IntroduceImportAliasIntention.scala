package org.jetbrains.plugins.scala.codeInsight.intention.imports

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo
import com.intellij.codeInsight.intention.{LowPriorityAction, PsiElementBaseIntentionAction}
import com.intellij.codeInsight.template.impl.TextExpression
import com.intellij.codeInsight.template.{Template, TemplateBuilderImpl, TemplateEditingAdapter, TemplateManager}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiFile}
import com.intellij.refactoring.util.CommonRefactoringUtil
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.codeInsight.intention.imports.IntroduceImportAliasIntention._
import org.jetbrains.plugins.scala.editor.typedHandler.ScalaTypedHandler
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.formatting.settings.ScalaCodeStyleSettings
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSimpleTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.usages.{ImportExprUsed, ImportSelectorUsed, ImportUsed, ImportWildcardSelectorUsed}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.{ScImportExpr, ScImportSelector, ScImportStmt}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.refactoring.ScalaNamesValidator
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil.BacktickedName
import org.jetbrains.plugins.scala.project.ProjectPsiElementExt

/**
 * Introduces an alias for an imported class, object or member and renames all its usages in the file
 * (port of Kotlin's `IntroduceImportAliasIntention`):
 * {{{
 *   import a.b.Foo             --> import a.b.{Foo => Foo1}   (Scala 2)
 *                              --> import a.b.Foo as Foo1     (Scala 3 / -Xsource:3)
 *   import a.b.{Foo, Baz}      --> import a.b.{Foo => Foo1, Baz}
 *   import a.b._               --> import a.b.{Foo => Foo1, _}
 * }}}
 * After the change an in-place rename template is started on the alias, so the user can type the final name.
 *
 * The intention is only available for references which are resolved through an import
 * (or on the import selector itself). References to local definitions, same-package definitions or
 * fully qualified references are not supported: there is no import to put the alias on.
 */
final class IntroduceImportAliasIntention extends PsiElementBaseIntentionAction with LowPriorityAction {

  override def getFamilyName: String = ScalaBundle.message("family.name.introduce.import.alias")

  override def getText: String = getFamilyName

  override def startInWriteAction(): Boolean = false

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findTarget(element).isDefined

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit = {
    val target = findTarget(element).getOrElse(return)
    val file = element.getContainingFile
    val aliasName = suggestAliasName(target)

    // One command, so that the alias and the in-place rename are undone together.
    // `TemplateBuilderImpl.buildInlineTemplate` modifies the document, so it needs the write action too.
    inWriteCommandAction {
      val result = applyAlias(target, aliasName)
      PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(editor.getDocument)
      startInplaceRename(project, editor, file, result, aliasName, target.name)
    }(project)
  }

  override def generatePreview(project: Project, editor: Editor, file: PsiFile): IntentionPreviewInfo = {
    val element = file.findElementAt(editor.getCaretModel.getOffset)
    findTarget(element) match {
      case Some(target) =>
        applyAlias(target, suggestAliasName(target))
        IntentionPreviewInfo.DIFF
      case None =>
        IntentionPreviewInfo.EMPTY
    }
  }
}

object IntroduceImportAliasIntention {

  /**
   * @param importElement the import selector or import expression which brings `name` into scope
   * @param name          imported name (as written in the source, i.e. with backticks if any)
   * @param usages        unqualified references which are resolved through `importElement`
   */
  private final case class Target(importElement: PsiElement, importExpr: ScImportExpr, name: String, usages: Seq[ScReference])

  private final case class Result(aliasId: PsiElement, usageIds: Seq[PsiElement])

  private def findTarget(element: PsiElement): Option[Target] = {
    if (element == null || !element.isValid) return None
    val ref = PsiTreeUtil.getParentOfType(element, classOf[ScReference], false)
    if (ref == null || ref.nameId != element) return None

    val importElementOpt = ScalaPsiUtil.getParentImportExpression(ref) match {
      case null => importElementForUsage(ref)
      case expr => importElementForImportReference(ref, expr)
    }

    for {
      importElement <- importElementOpt
      importExpr <- importExprOf(importElement)
      if importExpr.getParent.is[ScImportStmt]
      usages = findUsages(importElement, importExpr, ref.refName)
      if usages.nonEmpty
    } yield Target(importElement, importExpr, ref.nameId.getText, usages)
  }

  private def importExprOf(importElement: PsiElement): Option[ScImportExpr] = importElement match {
    case expr: ScImportExpr => Some(expr)
    case selector: ScImportSelector => Option(selector.parentImportExpression)
    case _ => None
  }

  /** Caret is on the last segment of an import expression or on a non-aliased import selector */
  private def importElementForImportReference(ref: ScReference, expr: ScImportExpr): Option[PsiElement] =
    ref.getParent match {
      case selector: ScImportSelector if selector.reference.contains(ref) && !selector.isAliasedImport && !selector.isGivenSelector =>
        Some(selector)
      case `expr` if expr.reference.contains(ref) && expr.selectorSet.isEmpty && !expr.hasWildcardSelector =>
        Some(expr)
      case _ =>
        None
    }

  /** Caret is on an unqualified reference which is resolved through an import */
  private def importElementForUsage(ref: ScReference): Option[PsiElement] = {
    if (ref.qualifier.isDefined) return None
    ref.multiResolveScala(false).iterator
      .flatMap(_.importsUsed)
      .flatMap(importElementImportingName(_, ref.refName))
      .nextOption()
  }

  private def importElementImportingName(importUsed: ImportUsed, name: String): Option[PsiElement] = importUsed match {
    case ImportSelectorUsed(selector)
      if !selector.isAliasedImport && selector.reference.exists(_.refName == name) =>
      Some(selector)
    case ImportExprUsed(expr) if expr.hasWildcardSelector =>
      Some(expr)
    case ImportExprUsed(expr) if expr.reference.exists(_.refName == name) =>
      Some(expr)
    case ImportWildcardSelectorUsed(expr) =>
      Some(expr)
    case _ =>
      None
  }

  private def findUsages(importElement: PsiElement, importExpr: ScImportExpr, name: String): Seq[ScReference] =
    importExpr.getContainingFile
      .depthFirst()
      .filterByType[ScReference]
      .filter { ref =>
        ref.refName == name &&
          ref.qualifier.isEmpty &&
          !PsiTreeUtil.isAncestor(importExpr, ref, false) &&
          ref.multiResolveScala(false).exists(_.importsUsed.exists(_.element == importElement))
      }
      .toSeq

  private def suggestAliasName(target: Target): String = {
    val file = target.importExpr.getContainingFile
    val strippedName = BacktickedName.stripBackticks(target.name)
    val baseName = if (ScalaNamesValidator.isIdentifier(s"${strippedName}1")) strippedName else "alias"
    val usedNames = namesUsedInFile(file)
    val contextRef = target.usages.head

    Iterator.from(1)
      .map(i => s"$baseName$i")
      .find(candidate => !usedNames.contains(candidate) && !resolvesAt(candidate, contextRef))
      .get
  }

  private def namesUsedInFile(file: PsiFile): Set[String] =
    file.depthFirst().collect {
      case named: ScNamedElement => named.name
      case ref: ScReference if ref.qualifier.isEmpty => ref.refName
      case selector: ScImportSelector if selector.importedName.isDefined => selector.importedName.get
    }.toSet

  /** Whether `name` would resolve to something (as an expression or as a type) at the place of `place` */
  private def resolvesAt(name: String, place: PsiElement): Boolean = {
    val context = place.getContext
    def resolvesAsExpr = ScalaPsiElementFactory.safe(_.createExpressionWithContextFromText(name, context, place)).exists {
      case ref: ScReferenceExpression => ref.multiResolveScala(false).nonEmpty
      case _ => false
    }
    def resolvesAsType = ScalaPsiElementFactory.safe(_.createTypeElementFromText(name, context, place)).exists {
      case ScSimpleTypeElement(ref) => ref.multiResolveScala(false).nonEmpty
      case _ => false
    }
    resolvesAsExpr || resolvesAsType
  }

  private def useScala3Syntax(element: PsiElement): Boolean = {
    val features = element.features
    val forceScala2 = ScalaCodeStyleSettings.getInstance(element.getProject).isForceScala2ImportSyntaxInSource3
    features.`Scala 3 renaming imports` && (features.isScala3 || !forceScala2)
  }

  private def renameArrow(element: PsiElement): String =
    if (useScala3Syntax(element)) "as"
    else if (ScalaCodeStyleSettings.getInstance(element.getProject).REPLACE_CASE_ARROW_WITH_UNICODE_CHAR) ScalaTypedHandler.unicodeCaseArrow
    else "=>"

  private def applyAlias(target: Target, aliasName: String): Result = {
    implicit val project: Project = target.importExpr.getProject
    val Target(importElement, importExpr, name, usages) = target

    val usageIds = usages.filter(_.isValid).map { usage =>
      usage.nameId.replace(ScalaPsiElementFactory.createIdentifier(aliasName).getPsi)
    }

    val arrow = renameArrow(importExpr)
    val renamedSelector = s"$name $arrow $aliasName"
    val exprText = importExpr.getText
    val exprStart = importExpr.getTextRange.getStartOffset
    def qualifierText = importExpr.qualifier.map(_.getText).getOrElse("")

    val newExprText = importElement match {
      case selector: ScImportSelector =>
        val range = selector.getTextRange.shiftLeft(exprStart)
        range.replace(exprText, renamedSelector)
      case expr: ScImportExpr if expr.hasWildcardSelector =>
        val wildcard = expr.wildcardElement.getOrElse(return Result(null, usageIds))
        expr.selectorSet match {
          case Some(_) =>
            val offset = wildcard.getTextRange.getStartOffset - exprStart
            exprText.substring(0, offset) + renamedSelector + ", " + exprText.substring(offset)
          case None =>
            s"$qualifierText.{$renamedSelector, ${wildcard.getText}}"
        }
      case _ =>
        if (arrow == "as") s"$qualifierText.$renamedSelector"
        else s"$qualifierText.{$renamedSelector}"
    }

    val newExpr = ScalaPsiElementFactory.createImportExprFromText(newExprText, importExpr, escapeKeywords = false)
    val replaced = importExpr.replace(newExpr).asInstanceOf[ScImportExpr]
    val aliasId = replaced.selectors.iterator
      .filter(_.aliasName.contains(aliasName))
      .flatMap(_.aliasNameElement)
      .nextOption()
      .orNull
    Result(aliasId, usageIds)
  }

  private def startInplaceRename(project: Project, editor: Editor, file: PsiFile, result: Result, aliasName: String, originalName: String): Unit = {
    val Result(aliasId, usageIds) = result
    if (aliasId == null || !aliasId.isValid || editor.isViewer) return

    val primaryVar = "ALIAS"
    val builder = new TemplateBuilderImpl(file)
    builder.replaceElement(aliasId, primaryVar, new TextExpression(aliasName), true)
    usageIds.filter(_.isValid).zipWithIndex.foreach { case (id, idx) =>
      builder.replaceElement(id, s"${primaryVar}_$idx", primaryVar, false)
    }

    val aliasPointer = aliasId.createSmartPointer
    // names which would clash with the alias (the suggested alias itself and the original name are free to use)
    val takenNames = namesUsedInFile(file) - aliasName - BacktickedName.stripBackticks(originalName)
    editor.getCaretModel.moveToOffset(file.getTextRange.getStartOffset)
    val template = builder.buildInlineTemplate()
    TemplateManager.getInstance(project).startTemplate(editor, template, new TemplateEditingAdapter {
      override def templateFinished(template: Template, brokenOff: Boolean): Unit = {
        val newName = Option(aliasPointer.getElement).map(_.getText).getOrElse(return)
        if (!ScalaNamesValidator.isIdentifier(newName))
          showError(project, editor, ScalaBundle.message("introduce.import.alias.invalid.name", newName))
        else if (takenNames.contains(BacktickedName.stripBackticks(newName)))
          showError(project, editor, ScalaBundle.message("introduce.import.alias.name.conflict", newName))
      }
    })
  }

  private def showError(project: Project, editor: Editor, @Nls message: String): Unit =
    CommonRefactoringUtil.showErrorHint(project, editor, message, ScalaBundle.message("family.name.introduce.import.alias"), null)
}
