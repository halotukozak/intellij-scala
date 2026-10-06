package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.{DumbAware, DumbService, Project}
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiComment, PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition, ScVariableDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createElementFromText
import org.jetbrains.plugins.scala.lang.refactoring.ScalaNamesValidator
import org.jetbrains.plugins.scala.lang.refactoring.namesSuggester.NameSuggester
import org.jetbrains.plugins.scala.lang.refactoring.util.{ScalaNamesUtil, ScalaVariableValidator}
import org.jetbrains.plugins.scala.project.{ProjectContext, ProjectExt, ScalaFeatures}

/**
 * Converts a `match` expression with simple patterns (literals, stable identifiers, type tests,
 * alternatives and wildcards, optionally with guards) to an `if / else if / else` chain:
 * {{{
 *   x match {
 *     case 1 => a
 *     case 2 | 3 => b
 *     case _: String => c
 *     case _ => d
 *   }
 * }}}
 * becomes
 * {{{
 *   if (x == 1) a
 *   else if (x == 2 || x == 3) b
 *   else if (x.isInstanceOf[String]) c
 *   else d
 * }}}
 *
 * A literal or a simple path to values is repeated in every condition. Any other scrutinee, e.g. a method call
 * like `builder.getTokenText`, would be evaluated several times, so it is first extracted to a local value.
 * When the `match` is a statement of a block, the value is defined right before it; otherwise (e.g. the `match` is
 * the whole body of a method) the value and the `if` chain are wrapped in a new block, like Introduce Variable does:
 * {{{
 *   def f(builder: Builder) = {
 *     val text = builder.getTokenText
 *     if (text == "a") ... else ...
 *   }
 * }}}
 * In dumb mode references cannot be resolved, so simple paths are always repeated and other scrutinees are not supported.
 */
final class MatchToIfIntention extends PsiElementBaseIntentionAction with DumbAware {

  import MatchToIfIntention._

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.convert.match.to.if")

  override def getText: String = getFamilyName

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean = {
    val matchStmt = PsiTreeUtil.getParentOfType(element, classOf[ScMatch], false)
    if (matchStmt == null) return false

    val clauses = matchStmt.caseClauses.orNull
    if (clauses == null) return false

    val offset = editor.getCaretModel.getOffset
    if (offset < matchStmt.getTextRange.getStartOffset || offset > clauses.getTextRange.getStartOffset) return false

    conversion(matchStmt, project).isDefined
  }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit = {
    val matchStmt = PsiTreeUtil.getParentOfType(element, classOf[ScMatch], false)
    if (matchStmt == null || !matchStmt.isValid) return

    conversion(matchStmt, project).foreach { case Conversion(subjectUsage, ifText) =>
      implicit val ctx: ProjectContext = project
      implicit val features: ScalaFeatures = ScalaFeatures.forPsiOrDefault(matchStmt)

      def suggestName(subject: ScExpression, container: PsiElement): String = {
        val validator = new ScalaVariableValidator(matchStmt, false, container, container)
        NameSuggester.suggestNames(subject, validator).headOption.getOrElse("value")
      }

      def createIf(subjectText: String): ScIf =
        ScalaPsiUtil.convertIfToBracelessIfNeeded(createElementFromText[ScIf](ifText(subjectText), features), recursive = true)

      val codeStyleManager = CodeStyleManager.getInstance(project)

      subjectUsage match {
        case Repeat(text) =>
          val newIf = createIf(text)
          IntentionPreviewUtils.write { () =>
            val replaced = matchStmt.replaceExpression(newIf, removeParenthesis = true)
            codeStyleManager.reformat(replaced)
          }
        case ExtractToValue(subject, block) =>
          val name = suggestName(subject, block)
          val definition = createElementFromText[ScPatternDefinition](s"val $name = ${subject.getText}", features)
          val newIf = createIf(name)
          IntentionPreviewUtils.write { () =>
            val added = ScalaPsiUtil.addStatementBefore(definition, block, Some(matchStmt))
            codeStyleManager.reformat(added)
            val replaced = matchStmt.replaceExpression(newIf, removeParenthesis = true)
            codeStyleManager.reformat(replaced)
          }
        case WrapInBlock(subject) =>
          val name = suggestName(subject, matchStmt)
          val newBlock = createElementFromText[ScBlockExpr](s"{\nval $name = ${subject.getText}\n${ifText(name)}\n}", features)
          newBlock.lastStatement.foreach {
            case oldIf: ScIf => oldIf.replace(ScalaPsiUtil.convertIfToBracelessIfNeeded(oldIf, recursive = true))
            case _ =>
          }
          val braceless = project.indentationBasedSyntaxEnabled(features) && allowsBracelessBlock(matchStmt)
          IntentionPreviewUtils.write { () =>
            val replaced = matchStmt.replaceExpression(newBlock, removeParenthesis = true)
            val reformatted = codeStyleManager.reformat(replaced)
            reformatted match {
              case block: ScBlockExpr if braceless =>
                ScalaPsiUtil.convertBlockToBraceless(block)
                codeStyleManager.reformat(block.getParent)
              case _ =>
            }
          }
      }

      IntentionPreviewUtils.write { () =>
        PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument)
      }
    }
  }
}

object MatchToIfIntention {

  import IfMatchConversionUtils._

  /** @param condition `None` stands for the final `else` */
  private final case class Branch(condition: Option[String => String], body: Body, forceBraces: Boolean)

  private sealed trait SubjectUsage

  /** The scrutinee is repeated in every condition */
  private final case class Repeat(text: String) extends SubjectUsage

  /** The scrutinee is extracted to a local value defined in `block` before the `if` chain */
  private final case class ExtractToValue(subject: ScExpression, block: ScBlock) extends SubjectUsage

  /** The `match` is replaced by a new block with a local value for the scrutinee followed by the `if` chain */
  private final case class WrapInBlock(subject: ScExpression) extends SubjectUsage

  /** @param ifText text of the resulting `if` chain for the given subject text */
  private final case class Conversion(subjectUsage: SubjectUsage, ifText: String => String)

  private def subjectUsage(matchStmt: ScMatch, subject: ScExpression, project: Project): Option[SubjectUsage] = {
    val isDumb = DumbService.isDumb(project)
    if (subject.is[ScLiteral] || isSimplePath(subject) && (isDumb || !pathContainsCall(subject)))
      Some(Repeat(subject.getText))
    else if (isDumb) None
    else matchStmt.getParent match {
      case block: ScBlock => Some(ExtractToValue(subject, block))
      case _ => Some(WrapInBlock(subject))
    }
  }

  /** Whether a block replacing `expr` may be written without braces in Scala 3 indentation-based syntax */
  private def allowsBracelessBlock(expr: ScExpression): Boolean = expr.getParent match {
    case function: ScFunctionDefinition => function.hasAssign && function.body.contains(expr)
    case definition: ScPatternDefinition => definition.expr.contains(expr)
    case definition: ScVariableDefinition => definition.expr.contains(expr)
    case _ => false
  }

  private def conversion(matchStmt: ScMatch, project: Project): Option[Conversion] = {
    val subject = matchStmt.expression.map(stripParentheses).orNull
    if (subject == null) return None
    val usage = subjectUsage(matchStmt, subject, project).orNull
    if (usage == null) return None

    val clauses = matchStmt.clauses
    if (clauses.isEmpty) return None
    if (clauses.exists(c => c.pattern.exists(containsComments) || c.guard.exists(containsComments))) return None

    val danglingComments = collectDanglingComments(matchStmt)

    val branches = clauses.zipWithIndex.map { case (clause, idx) =>
      val isLast = idx == clauses.size - 1
      for {
        pattern <- clause.pattern
        patternCondition <- condition(pattern)
        guard = clause.guard.flatMap(_.expr).map(guardText)
        if clause.guard.isEmpty || guard.isDefined
        branchCondition = (patternCondition, guard) match {
          case (Some(p), Some(g)) => Some((subject: String) => s"${parenthesizeOr(p(subject))} && $g")
          case (Some(p), None) => Some(p)
          case (None, Some(g)) => Some((_: String) => g)
          case (None, None) => None
        }
        // an unconditional case is possible only as the final `else`
        if branchCondition.isDefined || isLast
      } yield {
        val body = clause.expr.fold(Body.empty)(bodyOf).withLeadingComments(danglingComments.getOrElse(clause, Seq.empty))
        Branch(branchCondition, body, clause.expr.exists(isUnsafeWithoutBraces))
      }
    }

    if (branches.exists(_.isEmpty)) return None
    val allBranches = branches.flatten
    if (allBranches.head.condition.isEmpty) return None

    // without a default case, the result of `match` must not be used, otherwise the type of the expression changes
    if (allBranches.last.condition.isDefined && !isInStatementPosition(matchStmt)) return None

    Some(Conversion(usage, ifText(allBranches, _)))
  }

  private def ifText(branches: Seq[Branch], subject: String): String = {
    val useBraces = branches.exists(b => !b.body.isSingleLine || b.forceBraces)

    val parts = branches.map { branch =>
      val header = branch.condition.fold("")(c => s"if (${c(subject)})")
      val bodyText =
        if (useBraces) {
          if (branch.body.isEmpty) "{}"
          else ("{" +: indent(branch.body.lines, "  ") :+ "}").mkString("\n")
        } else branch.body.lines.head
      Seq(header, bodyText).filter(_.nonEmpty).mkString(" ")
    }

    parts.mkString(if (useBraces) " else " else "\nelse ")
  }

  /** @return `Some(None)` for an unconditional pattern, `Some(Some(condition))` for a condition on the subject */
  private def condition(pattern: ScPattern): Option[Option[String => String]] = pattern match {
    case ScParenthesisedPattern(inner) => condition(inner)
    case _: ScWildcardPattern => Some(None)
    case literal: ScLiteralPattern => Some(Some(subject => s"$subject == ${literal.getText}"))
    case ScStableReferencePattern(ref) => Some(Some(subject => s"$subject == ${referenceText(ref)}"))
    case typed: ScTypedPattern if typed.isWildcard =>
      typed.typePattern.map(tp => Some(subject => s"$subject.isInstanceOf[${tp.typeElement.getText}]"))
    case Sc3TypedPattern(_: ScWildcardPattern, typeElement) =>
      Some(Some(subject => s"$subject.isInstanceOf[${typeElement.getText}]"))
    case composite: ScCompositePattern =>
      val alternatives = composite.subpatterns.map(condition)
      if (alternatives.isEmpty || alternatives.exists(_.isEmpty)) None
      else if (alternatives.exists(_.contains(None))) Some(None)
      else {
        val conditions = alternatives.flatten.flatten
        Some(Some(subject => conditions.map(_(subject)).mkString(" || ")))
      }
    case _ => None
  }

  private def referenceText(ref: ScReferenceExpression): String = {
    val text = ref.getText
    if (ref.qualifier.isEmpty && ScalaNamesUtil.BacktickedName.hasBackticks(text)) {
      val stripped = ScalaNamesUtil.BacktickedName.stripBackticks(text)
      if (ScalaNamesValidator.isIdentifier(stripped) && !ScalaNamesValidator.isKeyword(stripped)) stripped else text
    } else text
  }

  private def parenthesizeOr(condition: String): String =
    if (condition.contains(" || ")) s"($condition)" else condition

  private def guardText(guard: ScExpression): String = {
    val needsParentheses = guard match {
      case ScInfixExpr(_, op, _) => op.refName != "&&" && ParserUtils.priority(op.refName, assignments = true) >= ParserUtils.priority("&&")
      case _: ScReferenceExpression | _: ScMethodCall | _: ScGenericCall | _: ScLiteral |
           _: ScParenthesisedExpr | _: ScPrefixExpr | _: ScThisReference => false
      case _ => true
    }
    if (needsParentheses) s"(${guard.getText})" else guard.getText
  }

  private def isUnsafeWithoutBraces(expr: ScExpression): Boolean = expr match {
    case block: ScBlock if !block.hasLBrace =>
      block.statements match {
        case Seq(single: ScExpression) => isUnsafeWithoutBraces(single)
        case _ => false
      }
    case _: ScIf | _: ScFunctionExpr | _: ScTry | _: ScWhile | _: ScFor => true
    case _ => false
  }

  /**
   * Comments located outside of case clause bodies, mapped to the clause they are moved to
   * (the following one, or the last one for trailing comments).
   */
  private def collectDanglingComments(matchStmt: ScMatch): Map[ScCaseClause, Seq[PsiComment]] = {
    val clauses = matchStmt.clauses
    val candidates =
      matchStmt.children.toSeq.filterByType[PsiComment] ++
        matchStmt.caseClauses.toSeq.flatMap(_.children.filterByType[PsiComment]) ++
        clauses.flatMap(_.children.filterByType[PsiComment])

    candidates
      .sortBy(_.getTextRange.getStartOffset)
      .groupBy { comment =>
        val offset = comment.getTextRange.getStartOffset
        clauses.find(_.getTextRange.getEndOffset > offset).getOrElse(clauses.last)
      }
  }

  private def isInStatementPosition(expr: ScExpression): Boolean = expr.getParent match {
    case block: ScBlock =>
      if (!block.lastStatement.contains(expr)) true
      else isInStatementPosition(block)
    case function: ScFunctionDefinition =>
      function.body.contains(expr) &&
        (!function.hasAssign || function.returnTypeElement.exists(_.getText == "Unit"))
    case ifStmt: ScIf if !ifStmt.condition.contains(expr) && ifStmt.elseExpression.isEmpty =>
      true
    case ifStmt: ScIf if !ifStmt.condition.contains(expr) =>
      isInStatementPosition(ifStmt)
    case whileStmt: ScWhile => whileStmt.expression.contains(expr)
    case doStmt: ScDo => doStmt.body.contains(expr)
    case forStmt: ScFor => !forStmt.isYield && forStmt.body.contains(expr)
    case _ => false
  }
}
