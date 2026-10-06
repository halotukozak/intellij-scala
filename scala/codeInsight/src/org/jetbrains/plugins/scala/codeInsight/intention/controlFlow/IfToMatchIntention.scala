package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.codeInsight.intention.preview.IntentionPreviewUtils
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi._
import com.intellij.psi.codeStyle.CodeStyleManager
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.codeInsight.ScalaCodeInsightBundle
import org.jetbrains.plugins.scala.codeInspection.typeChecking.IsInstanceOfCall
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.ScInterpolatedStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScNullLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScVariable
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypedDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText
import org.jetbrains.plugins.scala.project.{ProjectContext, ProjectExt, ScalaFeatures}

/**
 * Converts an `if / else if / else` chain, in which every condition compares the same expression,
 * to a `match` expression:
 * {{{
 *   if (x == 1) a else if (x == 2 || x == 3) b else if (x.isInstanceOf[String]) c else d
 * }}}
 * becomes
 * {{{
 *   x match {
 *     case 1 => a
 *     case 2 | 3 => b
 *     case _: String => c
 *     case _ => d
 *   }
 * }}}
 *
 * The subject may be a simple path or a chain of method calls with simple arguments
 * (e.g. `builder.getTokenText`, see [[IfMatchConversionUtils.isSubjectCandidate]]), which has to be
 * structurally equivalent in all conditions. Note that such a subject is then evaluated only once,
 * instead of once per comparison, which is the purpose of the conversion.
 */
final class IfToMatchIntention extends PsiElementBaseIntentionAction {

  import IfToMatchIntention._

  override def getFamilyName: String = ScalaCodeInsightBundle.message("family.name.convert.if.to.match")

  override def getText: String = getFamilyName

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean = {
    val ifStmt = PsiTreeUtil.getParentOfType(element, classOf[ScIf], false)
    if (ifStmt == null) return false

    val thenExpression = ifStmt.thenExpression.orNull
    if (thenExpression == null) return false

    val offset = editor.getCaretModel.getOffset
    if (offset < ifStmt.getTextRange.getStartOffset || offset > thenExpression.getTextRange.getStartOffset) return false

    conversion(topmostIf(ifStmt)).isDefined
  }

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit = {
    val ifStmt = PsiTreeUtil.getParentOfType(element, classOf[ScIf], false)
    if (ifStmt == null || !ifStmt.isValid) return

    val topIf = topmostIf(ifStmt)
    conversion(topIf).foreach { text =>
      implicit val ctx: ProjectContext = project
      val newMatch = createExpressionFromText(text, ScalaFeatures.forPsiOrDefault(topIf))

      IntentionPreviewUtils.write { () =>
        val replaced = topIf.replaceExpression(newMatch, removeParenthesis = true)
        CodeStyleManager.getInstance(project).reformat(replaced)
        PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument)
      }
    }
  }
}

object IfToMatchIntention {

  import IfMatchConversionUtils._

  private final case class Branch(patterns: Seq[String], guard: Option[String], body: Body)

  private def topmostIf(ifStmt: ScIf): ScIf = ifStmt.getParent match {
    case parent: ScIf if parent.elseExpression.contains(ifStmt) => topmostIf(parent)
    case _ => ifStmt
  }

  /** @return text of the resulting match expression, if the conversion is possible */
  private def conversion(topIf: ScIf): Option[String] = {
    val (ifs, finalElse) = collectChain(topIf)
    if (ifs.exists(i => i.condition.isEmpty || i.thenExpression.isEmpty)) return None

    val conditions = ifs.flatMap(_.condition)
    if (conditions.exists(containsComments)) return None

    val subjects = subjectCandidates(conditions.head)
    subjects.iterator.flatMap { subject =>
      val parsed = conditions.map(parseCondition(_, subject))
      if (parsed.forall(_.isDefined)) Some(subject -> parsed.flatten)
      else None
    }.nextOption().map { case (subject, parsedConditions) =>
      val branches = ifs.zip(parsedConditions).zipWithIndex.map { case ((ifStmt, (patterns, guard)), idx) =>
        val previousIf = if (idx == 0) None else Some(ifs(idx - 1))
        val comments = previousIf.toSeq.flatMap(commentsAfterThen) ++ commentsBeforeThen(ifStmt)
        Branch(patterns, guard, bodyOf(ifStmt.thenExpression.get).withLeadingComments(comments))
      }

      // without the final `else`, `case _ =>` is still required to avoid `MatchError`
      val defaultComments = commentsAfterThen(ifs.last)
      val defaultBody = finalElse.fold(Body.empty)(bodyOf).withLeadingComments(defaultComments)
      val default = Branch(Seq("_"), None, defaultBody)

      matchText(subject.getText, branches :+ default, topIf)
    }
  }

  private def collectChain(topIf: ScIf): (Seq[ScIf], Option[ScExpression]) = {
    val ifs = Seq.newBuilder[ScIf]
    var current = topIf
    var finalElse: Option[ScExpression] = None
    var continue = true
    while (continue) {
      ifs += current
      current.elseExpression match {
        case Some(next: ScIf) => current = next
        case other =>
          finalElse = other
          continue = false
      }
    }
    (ifs.result(), finalElse)
  }

  private def commentsBeforeThen(ifStmt: ScIf): Seq[PsiComment] = {
    val thenStart = ifStmt.thenExpression.map(_.getTextRange.getStartOffset).getOrElse(Int.MaxValue)
    ifStmt.children.toSeq.filterByType[PsiComment].filter(_.getTextRange.getStartOffset < thenStart)
  }

  private def commentsAfterThen(ifStmt: ScIf): Seq[PsiComment] = {
    val thenEnd = ifStmt.thenExpression.map(_.getTextRange.getEndOffset).getOrElse(Int.MaxValue)
    ifStmt.children.toSeq.filterByType[PsiComment].filter(_.getTextRange.getStartOffset >= thenEnd)
  }

  private def matchText(subject: String, branches: Seq[Branch], context: PsiElement): String = {
    val braceless = context.getProject.indentationBasedSyntaxEnabled(ScalaFeatures.forPsiOrDefault(context))

    val clauses = branches.flatMap { case Branch(patterns, guard, body) =>
      val header = "case " + patterns.mkString(" | ") + guard.fold("")(" if " + _) + " =>"
      if (body.isEmpty) Seq(header)
      else if (body.isSingleLine) Seq(header + " " + body.lines.head)
      else header +: indent(body.lines, "  ")
    }

    if (braceless) (s"$subject match" +: indent(clauses, "  ")).mkString("\n")
    else (s"$subject match {" +: indent(clauses, "  ") :+ "}").mkString("\n")
  }

  /** Expressions which can be used as a match subject, taken from the first comparison of the condition */
  private def subjectCandidates(condition: ScExpression): Seq[ScExpression] = stripParentheses(condition) match {
    case ScInfixExpr(left, op, _) if op.refName == "||" || op.refName == "&&" =>
      subjectCandidates(left)
    case ScInfixExpr(left, op, right) if isEquality(op) =>
      Seq(left, right).map(stripParentheses).filter(isSubjectCandidate)
    case IsInstanceOfCall(call) =>
      instanceOfSubject(call).filter(isSubjectCandidate).toSeq
    case _ => Seq.empty
  }

  /** @return patterns and an optional guard */
  private def parseCondition(condition: ScExpression, subject: ScExpression): Option[(Seq[String], Option[String])] = {
    val operands = andOperands(condition)
    patterns(operands.head, subject).map { patterns =>
      val guard = operands.tail match {
        case Seq() => None
        case rest => Some(rest.map(_.getText).mkString(" && "))
      }
      (patterns, guard)
    }
  }

  private def andOperands(expr: ScExpression): Seq[ScExpression] = expr match {
    case ScInfixExpr(left, op, right) if op.refName == "&&" => andOperands(left) :+ right
    case ScParenthesisedExpr(inner) =>
      val operands = andOperands(inner)
      if (operands.size == 1) Seq(expr) else operands
    case _ => Seq(expr)
  }

  private def patterns(condition: ScExpression, subject: ScExpression): Option[Seq[String]] = stripParentheses(condition) match {
    case ScInfixExpr(left, op, right) if op.refName == "||" =>
      for {
        l <- patterns(left, subject)
        r <- patterns(right, subject)
      } yield l ++ r
    case ScInfixExpr(left, op, right) if isEquality(op) =>
      val (l, r) = (stripParentheses(left), stripParentheses(right))
      val other =
        if (sameExpression(l, subject)) Some(r)
        else if (sameExpression(r, subject)) Some(l)
        else None
      other
        .filter(o => op.refName == "==" || o.is[ScNullLiteral])
        .flatMap(patternText)
        .map(Seq(_))
    case IsInstanceOfCall(call) if instanceOfSubject(call).exists(sameExpression(_, subject)) =>
      call.typeArguments match {
        case Seq(typeArg) => Some(Seq(s"_: ${typeArg.getText}"))
        case _ => None
      }
    case _ => None
  }

  private def isEquality(op: ScReferenceExpression): Boolean =
    op.refName == "==" || op.refName == "eq"

  private def instanceOfSubject(call: ScGenericCall): Option[ScExpression] = call.referencedExpr match {
    case ref: ScReferenceExpression => ref.qualifier.map(stripParentheses)
    case _ => None
  }

  private def patternText(expr: ScExpression): Option[String] = expr match {
    case _: ScInterpolatedStringLiteral => None
    case literal: ScLiteral => Some(literal.getText)
    case prefix@ScPrefixExpr(op, literal: ScLiteral)
      if op.refName == "-" && literal.getText.headOption.exists(_.isDigit) => Some(prefix.getText)
    case ref: ScReferenceExpression if isStableIdentifier(ref) =>
      val text = ref.getText
      val needsBackticks = ref.qualifier.isEmpty && !text.startsWith("`") &&
        text.headOption.exists(c => c.isLower || c == '_')
      Some(if (needsBackticks) s"`$text`" else text)
    case _ => None
  }

  private def isStableIdentifier(ref: ScReferenceExpression): Boolean = {
    val qualifierIsStable = ref.qualifier match {
      case None => true
      case Some(q: ScReferenceExpression) => isStableQualifier(q)
      case Some(_: ScThisReference | _: ScSuperReference) => true
      case _ => false
    }
    qualifierIsStable && isStableElement(ref.resolve())
  }

  private def isStableQualifier(ref: ScReferenceExpression): Boolean = ref.resolve() match {
    case _: PsiPackage => true
    case c: PsiClass if !c.is[ScObject] => ref.qualifier.forall(_.is[ScReferenceExpression]) // java class with static members
    case _ => isStableIdentifier(ref)
  }

  private def isStableElement(element: PsiElement): Boolean = element match {
    case _: ScObject => true
    case definition: ScTypedDefinition =>
      definition.isStable && !definition.isVar && !definition.nameContext.is[ScVariable]
    case _: PsiEnumConstant => true
    case field: PsiField => field.hasModifierProperty(PsiModifier.FINAL) && field.hasModifierProperty(PsiModifier.STATIC)
    case _ => false
  }
}
