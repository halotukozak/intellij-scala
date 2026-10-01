package org.jetbrains.plugins.scala.codeInsight.intention.matcher

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.TextRange
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiClass, PsiDocumentManager, PsiElement, PsiNamedElement, SmartPointerManager}
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.completion.clauses.{ClassPatternComponents, ClauseCompletionParameters, PatternGenerationStrategy, StablePatternComponents}
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScPrimaryConstructor, ScStableCodeReference}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScMatch
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.api.ExtractClass
import org.jetbrains.plugins.scala.lang.psi.types.{Context, ScType, TypePresentationContext}

/**
 * Adds case clauses for the values of a sealed hierarchy / enumeration that an existing `match` does not handle yet.
 * The new clauses are inserted before a trailing catch-all clause (`case _ =>`), if there is one.
 */
final class AddRemainingCasesIntention extends PsiElementBaseIntentionAction {

  import AddRemainingCasesIntention._

  override def getFamilyName: String = ScalaBundle.message("family.name.add.remaining.cases")

  override def getText: String = getFamilyName

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    findMatch(element).exists(remainingCases(_).isDefined)

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    for {
      matchStmt <- findMatch(element)
      RemainingCases(strategy, missing, anchor) <- remainingCases(matchStmt)
    } addClauses(matchStmt, strategy, missing, anchor)(project, editor)
}

object AddRemainingCasesIntention {

  private final case class RemainingCases(strategy: PatternGenerationStrategy,
                                          missing: Seq[ClassPatternComponents],
                                          insertBefore: Option[ScCaseClause])

  private sealed trait Coverage
  private case object CoversAll extends Coverage
  private final case class CoversClass(`class`: PsiClass) extends Coverage
  private final case class CoversValue(element: PsiNamedElement) extends Coverage

  /** The innermost `match` around the caret, unless the caret is inside one of its case bodies. */
  private def findMatch(element: PsiElement): Option[ScMatch] =
    PsiTreeUtil.getParentOfType(element, classOf[ScMatch], false) match {
      case null => None
      case matchStmt if matchStmt.clauses.exists(_.expr.exists(_.isAncestorOf(element))) => None
      case matchStmt => Some(matchStmt)
    }

  private def remainingCases(matchStmt: ScMatch): Option[RemainingCases] = {
    val clauses = matchStmt.clauses
    if (clauses.isEmpty) return None

    implicit val parameters: ClauseCompletionParameters =
      ClauseCompletionParameters(matchStmt, matchStmt.getContainingFile.getResolveScope)
    implicit val tpc: TypePresentationContext = TypePresentationContext(matchStmt)
    implicit val context: Context = Context(matchStmt)

    for {
      scrutinee <- matchStmt.expression
      scrutineeType <- scrutinee.`type`().toOption
      strategy <- PatternGenerationStrategy.unapply(scrutineeType)
      if strategy.canBeExhaustive

      catchAll = clauses.lastOption.filter(isCatchAll)
      coverage = clauses
        .filterNot(catchAll.contains)
        .filter(_.guard.isEmpty)
        .flatMap(_.pattern)
        .flatMap(coverageOf)
      if !coverage.contains(CoversAll)

      missing = strategy.patterns.collect {
        case components: ClassPatternComponents if !isCovered(components, coverage) => components
      }
      if missing.nonEmpty
    } yield RemainingCases(strategy, missing, catchAll)
  }

  private def isCatchAll(clause: ScCaseClause): Boolean =
    clause.guard.isEmpty && clause.pattern.exists(isTrivial)

  /** Patterns that match any value without inspecting it. */
  private def isTrivial(pattern: ScPattern): Boolean = pattern match {
    case _: ScWildcardPattern | _: ScSeqWildcardPattern | _: ScReferencePattern => true
    case ScNamingPattern(named) => isTrivial(named)
    case ScParenthesisedPattern(inner) => isTrivial(inner)
    case _ => false
  }

  private def coverageOf(pattern: ScPattern)(implicit context: Context): Seq[Coverage] = pattern match {
    case p if isTrivial(p) => Seq(CoversAll)
    case ScNamingPattern(named) => coverageOf(named)
    case ScParenthesisedPattern(inner) => coverageOf(inner)
    case composite: ScCompositePattern => composite.subpatterns.flatMap(coverageOf)
    case ScTypedPattern(typeElement) => classOfType(typeElement.`type`().toOption)
    case Sc3TypedPattern((inner, typeElement)) if isTrivial(inner) => classOfType(typeElement.`type`().toOption)
    case constructor: ScConstructorPattern if constructor.args.patterns.forall(isTrivial) =>
      extractorClass(constructor.ref).map(CoversClass).toSeq
    case infix: ScInfixPattern if isTrivial(infix.left) && infix.rightOption.forall(isTrivial) =>
      extractorClass(infix.operation).map(CoversClass).toSeq
    case ScStableReferencePattern(reference) =>
      reference.resolve() match {
        case obj: ScObject => Seq(CoversClass(obj))
        case named: PsiNamedElement => Seq(CoversValue(named))
        case _ => Seq.empty
      }
    case _ => Seq.empty
  }

  private def classOfType(`type`: Option[ScType])
                         (implicit context: Context): Seq[Coverage] =
    `type`.collect { case ExtractClass(clazz) => CoversClass(clazz) }.toSeq

  private def extractorClass(reference: ScStableCodeReference): Option[PsiClass] =
    reference.resolve() match {
      case constructor: ScPrimaryConstructor => Option(constructor.containingClass)
      case function: ScFunction => function.containingClass match {
        case obj: ScObject => ScalaPsiUtil.getCompanionModule(obj).orElse(Some(obj))
        case clazz => Option(clazz)
      }
      case clazz: PsiClass => Some(clazz)
      case _ => None
    }

  private def isCovered(components: ClassPatternComponents, coverage: Seq[Coverage]): Boolean = {
    val target = components.targetClass
    val isEnumConstant = components.is[StablePatternComponents] &&
      components.targetQualifiedName != target.qualifiedName

    coverage.exists {
      case CoversAll => true
      case CoversClass(clazz) if isEnumConstant => sameClass(clazz, target)
      case CoversClass(clazz) => sameClass(clazz, target) || target.isInheritor(clazz, true)
      case CoversValue(named) if isEnumConstant => components.targetQualifiedName.endsWith("." + named.name)
      case CoversValue(named) => named == target
    }
  }

  private def sameClass(left: PsiClass, right: PsiClass): Boolean =
    left == right || left.getManager.areElementsEquivalent(left, right)

  private def addClauses(matchStmt: ScMatch,
                         strategy: PatternGenerationStrategy,
                         missing: Seq[ClassPatternComponents],
                         insertBefore: Option[ScCaseClause])
                        (implicit project: Project, editor: Editor): Unit = {
    val document = editor.getDocument
    val documentManager = PsiDocumentManager.getInstance(project)
    documentManager.doPostponedOperationsAndUnblockDocument(document)

    val anchor = insertBefore.getOrElse(matchStmt.clauses.last)
    val anchorStart = anchor.getTextRange.getStartOffset
    val lineStart = document.getLineStartOffset(document.getLineNumber(anchorStart))
    val indent = document.getText(new TextRange(lineStart, anchorStart))
    val indentIsBlank = indent.forall(_.isWhitespace)

    val clausesText = missing.map(_.canonicalClauseText + " ???")
    val (offset, text) = insertBefore match {
      case Some(_) if indentIsBlank => (lineStart, clausesText.map(indent + _ + "\n").mkString)
      case Some(_) => (anchorStart, clausesText.map(_ + "\n").mkString)
      case None => (anchor.getTextRange.getEndOffset, clausesText.map("\n" + (if (indentIsBlank) indent else "") + _).mkString)
    }

    val matchPointer = SmartPointerManager.createPointer(matchStmt)
    document.insertString(offset, text)
    documentManager.commitDocument(document)

    for (updatedMatch <- Option(matchPointer.getElement)) {
      val insertedRange = TextRange.from(offset, text.length)
      val newClauses = updatedMatch.clauses.filter(clause => insertedRange.contains(clause.getTextRange))
      val firstNewPointer = newClauses.headOption.map(SmartPointerManager.createPointer(_))
      if (newClauses.size == missing.size) {
        strategy.adjustTypes(missing, newClauses)
      }

      val range = updatedMatch.getTextRange
      CodeStyleManager.getInstance(project).reformatText(updatedMatch.getContainingFile, range.getStartOffset, range.getEndOffset)
      documentManager.doPostponedOperationsAndUnblockDocument(document)

      for {
        pointer <- firstNewPointer
        firstNew <- Option(pointer.getElement)
        body <- firstNew.expr
      } {
        val bodyRange = body.getTextRange
        editor.getCaretModel.moveToOffset(bodyRange.getStartOffset)
        editor.getSelectionModel.setSelection(bodyRange.getStartOffset, bodyRange.getEndOffset)
      }
    }
  }
}
