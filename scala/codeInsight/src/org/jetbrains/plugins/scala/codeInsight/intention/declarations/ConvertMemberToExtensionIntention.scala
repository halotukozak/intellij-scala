package org.jetbrains.plugins.scala.codeInsight
package intention
package declarations

import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.codeInsight.intention.{LowPriorityAction, PsiElementBaseIntentionAction}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.{PsiElement, PsiMember, PsiModifier, PsiWhiteSpace}
import com.intellij.refactoring.util.CommonRefactoringUtil
import org.jetbrains.plugins.scala.codeInsight.intention.declarations.ConvertMemberToExtensionIntention._
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScInterpolatedStringLiteral, ScStableCodeReference}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScExtension, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScMember, ScObject, ScTemplateDefinition, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScModifierListOwner, ScNamedElement}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.{TemplateDefKind, TemplateDefinitionBuilder}
import org.jetbrains.plugins.scala.lang.psi.impl.search.ScalaOverridingMemberSearcher
import org.jetbrains.plugins.scala.lang.refactoring.ScalaNamesValidator
import org.jetbrains.plugins.scala.lang.scaladoc.psi.api.ScDocComment
import org.jetbrains.plugins.scala.project.ScalaFeatures

import scala.jdk.CollectionConverters._

/**
 * Converts a method of a class or trait into a Scala 3 extension method on that class.
 *
 * The extension is placed in the companion object: it is found through the implicit scope of the class,
 * so qualified call sites keep compiling without new imports,
 * and the companion object can still access private members of the class.
 */
final class ConvertMemberToExtensionIntention extends PsiElementBaseIntentionAction with LowPriorityAction {

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.convert.member.to.extension")

  override def getText: String = ScalaCodeInsightBundle.message("convert.member.to.extension")

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findTarget(element).isDefined

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    findTarget(element).foreach { target =>
      val isPreview = IntentionPreviewUtils.isPreviewElement(target.function)
      if (!isPreview && ScalaOverridingMemberSearcher.search(target.function, deep = false).nonEmpty) {
        val message = ScalaCodeInsightBundle.message("convert.member.to.extension.method.is.overridden", target.function.name)
        CommonRefactoringUtil.showErrorHint(project, editor, message, getText, null)
      } else {
        if (isPreview)
          convert(target)(project)
        else {
          qualifyUnqualifiedUsages(target.function, target.clazz)
          // usages qualified above may precede the method in the file, so the body edits have to be recomputed
          findTarget(target.function.nameId).foreach(convert(_)(project))
        }
      }
    }

  private def convert(target: Target)(implicit project: Project): Unit = {
    import target._

    implicit val features: ScalaFeatures = function

    val extensionText = createExtensionText(target)
    val added = clazz.baseCompanion match {
      case Some(companion: ScObject) =>
        val extension = createExtension(extensionText)
        companion.addMember(extension, None)
      case _ =>
        val companion = TemplateDefinitionBuilder(
          kind = TemplateDefKind.Object,
          name = clazz.name,
          body = "\n  " + extensionText + "\n",
          context = clazz.getContext,
          child = clazz
        ).createTemplateDefinition()
        val parent = clazz.getParent
        val addedCompanion = parent.addAfter(companion, clazz)
        parent.addAfter(ScalaPsiElementFactory.createNewLine("\n\n"), clazz)
        addedCompanion
    }

    function.getPrevSibling match {
      case whitespace: PsiWhiteSpace => whitespace.delete()
      case _ =>
    }
    function.delete()
    removeEmptyBracelessBody(clazz)
    added.depthFirst().collectFirst { case e: ScExtension => e }.foreach { extension =>
      extension.extensionMethods.foreach(_.getModifierList.setModifierProperty("final", false))
    }
    CodeStyleManager.getInstance(project).reformat(added)
  }
}

object ConvertMemberToExtensionIntention {

  private final case class Target(
    function: ScFunctionDefinition,
    clazz: ScTypeDefinition,
    receiverName: String,
    edits: Seq[(TextRange, String)]
  )

  private def findTarget(element: PsiElement): Option[Target] = element.getParent match {
    case function: ScFunctionDefinition
      if function.nameId == element &&
        function.isInScala3File &&
        isConvertible(function) =>
      function.containingClass match {
        case clazz: ScTypeDefinition if (clazz.is[ScClass] || clazz.is[ScTrait]) && !clazz.isLocal =>
          val receiver = chooseReceiverName(function, clazz)
          collectBodyEdits(function, clazz, receiver).map(Target(function, clazz, receiver, _))
        case _ => None
      }
    case _ => None
  }

  private def isConvertible(function: ScFunctionDefinition): Boolean =
    function.body.isDefined &&
      !function.isConstructor &&
      !function.isExtensionMethod &&
      !function.hasModifierPropertyScala("override") &&
      !function.hasModifierPropertyScala("implicit") &&
      function.getModifierList.accessModifier.isEmpty &&
      function.superMethods.isEmpty

  /**
   * Computes the text changes that make the method body valid outside the class:
   * unqualified member references and `this` are rewritten to use the receiver parameter.
   *
   * @return None if the method uses something an extension method can't reach
   *         (`super`, `private[this]` members, type members of the class, ...)
   */
  private def collectBodyEdits(
    function: ScFunctionDefinition,
    clazz: ScTypeDefinition,
    receiver: String
  ): Option[Seq[(TextRange, String)]] = {
    val edits = Seq.newBuilder[(TextRange, String)]
    val iterator = function.depthFirst(!_.is[ScDocComment])
    while (iterator.hasNext) {
      iterator.next() match {
        case _: ScSuperReference =>
          return None
        case thisRef: ScThisReference =>
          if (thisRef.refTemplate.contains(clazz))
            edits += thisRef.getTextRange -> wrapIfInterpolated(thisRef, receiver)
        case ref: ScReferenceExpression if ref.qualifier.isEmpty && !isOperation(ref) =>
          ref.bind() match {
            case Some(result) if !result.isNamedParameter && result.importsUsed.isEmpty && isInstanceMemberOf(clazz, result.element) =>
              if (isAccessibleOnlyFromThis(result.element))
                return None
              edits += ref.getTextRange -> wrapIfInterpolated(ref, s"$receiver.${ref.getText}")
            case _ =>
          }
        case ref: ScStableCodeReference if ref.qualifier.isEmpty =>
          val resolved = ref.resolve()
          // type members and stable paths would need a path-dependent prefix
          if (resolved != null && isInstanceMemberOf(clazz, resolved))
            return None
        case _ =>
      }
    }
    Some(edits.result())
  }

  private def wrapIfInterpolated(ref: PsiElement, text: String): String =
    if (ref.getParent.is[ScInterpolatedStringLiteral]) s"{$text}" else text

  private def isOperation(ref: ScReferenceExpression): Boolean = ref.getParent match {
    case call: ScSugarCallExpr => call.operation == ref
    case _ => false
  }

  private def memberOf(element: PsiElement): Option[PsiMember] = element match {
    case member: ScMember => Some(member)
    case named: ScNamedElement => named.nameContext.asOptionOf[ScMember]
    case member: PsiMember => Some(member)
    case _ => None
  }

  private def isInstanceMemberOf(clazz: ScTypeDefinition, element: PsiElement): Boolean =
    memberOf(element).exists { member =>
      val owner = member.getContainingClass
      owner != null &&
        !owner.is[ScObject] &&
        !member.hasModifierProperty(PsiModifier.STATIC) &&
        (owner == clazz || clazz.isInheritor(owner, true))
    }

  private def isAccessibleOnlyFromThis(element: PsiElement): Boolean = element match {
    case parameter: ScClassParameter =>
      !parameter.isClassMember || parameter.isPrivateThis
    case _ =>
      memberOf(element).exists {
        case owner: ScModifierListOwner =>
          owner.getModifierList.accessModifier.exists(_.isThis)
        case _ => false
      }
  }

  private def chooseReceiverName(function: ScFunctionDefinition, clazz: ScTypeDefinition): String = {
    val className = clazz.name
    val base =
      if (className.nonEmpty && ScalaNamesValidator.isIdentifier(className) && className.head.isLetter)
        className.head.toLower.toString + className.tail
      else "self"

    val usedNames = function.depthFirst().collect {
      case ref: ScReferenceExpression => ref.refName
      case named: ScNamedElement => named.name
    }.toSet

    (Iterator(base) ++ Iterator.from(1).map(base + _))
      .find(name => !usedNames.contains(name) && !ScalaNamesValidator.isKeyword(name))
      .get
  }

  private def createExtensionText(target: Target): String = {
    import target._

    val docComment = function.docComment
    val definitionStart = docComment
      .map(_.getNextSiblingNotWhitespace)
      .getOrElse(function)
      .startOffset
    val definitionEnd = function.endOffset

    val fileText = function.getContainingFile.getText
    val definitionText = edits
      .sortBy(-_._1.getStartOffset)
      .foldLeft(new StringBuilder(fileText.substring(definitionStart, definitionEnd))) {
        case (builder, (range, replacement)) =>
          builder.replace(range.getStartOffset - definitionStart, range.getEndOffset - definitionStart, replacement)
      }
      .toString

    val typeParameters = clazz.typeParameters
    val typeParameterClause =
      if (typeParameters.isEmpty) ""
      else typeParameters.map(_.getText.stripPrefix("+").stripPrefix("-")).mkString("[", ", ", "]")
    val receiverType =
      if (typeParameters.isEmpty) clazz.name
      else clazz.name + typeParameters.map(_.name).mkString("[", ", ", "]")

    val docText = docComment.fold("")(_.getText + "\n")
    s"${docText}extension $typeParameterClause($receiverName: $receiverType) $definitionText"
  }

  /** `class A:` without any statements left after it doesn't parse, so the colon has to go too */
  private def removeEmptyBracelessBody(clazz: ScTypeDefinition): Unit =
    clazz.extendsBlock.templateBody.foreach { body =>
      val colon = body.getColon
      val hasOnlyColonAndWhitespaces = colon.isDefined && body.getNode.getChildren(null).forall { node =>
        node.getPsi.is[PsiWhiteSpace] || colon.contains(node.getPsi)
      }
      if (hasOnlyColonAndWhitespaces)
        body.delete()
    }

  private def createExtension(text: String)(implicit features: ScalaFeatures, project: Project): ScExtension =
    ScalaPsiElementFactory.createScalaFileFromText(text, features)
      .depthFirst()
      .collectFirst { case extension: ScExtension => extension }
      .get

  /**
   * Unqualified calls inside the class and its subclasses can't see the extension method,
   * they need an explicit receiver: `foo()` becomes `this.foo()`.
   */
  private def qualifyUnqualifiedUsages(function: ScFunctionDefinition, clazz: ScTypeDefinition): Unit = {
    val references = ReferencesSearch.search(function, function.getUseScope).findAll().asScala.map(_.getElement)
    references.foreach {
      case ref: ScReferenceExpression
        if ref.qualifier.isEmpty &&
          !isOperation(ref) &&
          !function.isAncestorOf(ref) &&
          !ref.getParent.is[ScInterpolatedStringLiteral] =>
        thisQualifier(ref, clazz).foreach { qualifier =>
          val qualified = ScalaPsiElementFactory.createExpressionWithContextFromText(s"$qualifier.${ref.getText}", ref.getContext, ref)
          ref.replace(qualified)
        }
      case _ =>
    }
  }

  private def thisQualifier(ref: PsiElement, clazz: ScTypeDefinition): Option[String] = {
    def inherits(template: ScTemplateDefinition): Boolean =
      template == clazz || template.isInheritor(clazz, true)

    val templates = ref.parentsInFile.collect { case template: ScTemplateDefinition => template }.toSeq
    templates.headOption.flatMap { nearest =>
      if (inherits(nearest)) Some("this")
      else templates.collectFirst { case outer: ScTypeDefinition if inherits(outer) => s"${outer.name}.this" }
    }
  }
}
