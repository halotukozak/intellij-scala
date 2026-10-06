package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import com.intellij.codeInsight.PsiEquivalenceUtil
import com.intellij.psi.{PsiComment, PsiElement, PsiMethod, PsiWhiteSpace}
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr._

/**
 * Shared helpers for [[IfToMatchIntention]] and [[MatchToIfIntention]].
 */
private object IfMatchConversionUtils {

  /** The text of a branch body, split into lines, without the indentation of the original location. */
  final case class Body(lines: Seq[String]) {
    def isEmpty: Boolean = lines.isEmpty

    def isSingleLine: Boolean = lines.size == 1

    def withLeadingComments(comments: Seq[PsiComment]): Body =
      Body(comments.flatMap(c => linesOf(c, c)) ++ lines)
  }

  object Body {
    val empty: Body = Body(Seq.empty)
  }

  def isComment(e: PsiElement): Boolean = e.is[PsiComment]

  def containsComments(e: PsiElement): Boolean =
    e.is[PsiComment] || PsiTreeUtil.findChildOfType(e, classOf[PsiComment]) != null

  /** Body of the given expression; for a block with braces, only the content between braces is taken. */
  def bodyOf(expr: ScExpression): Body = expr match {
    case block: ScBlockExpr if block.isEnclosedByBraces =>
      val inner = block.children.toSeq
        .dropWhile(c => !block.getLBrace.contains(c)).drop(1)
        .takeWhile(c => !block.getRBrace.contains(c))
      bodyOfElements(inner)
    case block: ScBlock =>
      bodyOfElements(block.children.toSeq)
    case _ =>
      Body(linesOf(expr, expr))
  }

  def bodyOfElements(elements: Seq[PsiElement]): Body = {
    val meaningful = elements.filterNot(e => e.is[PsiWhiteSpace] || e.getTextLength == 0 || isSemicolon(e))
    if (meaningful.isEmpty) Body.empty
    else Body(linesOf(meaningful.head, meaningful.last))
  }

  private def isSemicolon(e: PsiElement): Boolean = e.getText == ";" && e.getFirstChild == null

  /** Lines of the text between the start of `first` and the end of `last`, de-indented by the column of `first`. */
  private def linesOf(first: PsiElement, last: PsiElement): Seq[String] = {
    val fileText = first.getContainingFile.getText
    val start = first.getTextRange.getStartOffset
    val end = last.getTextRange.getEndOffset
    val lineStart = fileText.lastIndexOf('\n', start - 1) + 1
    val baseIndent = start - lineStart

    val lines = fileText.substring(start, end).split("\n", -1).toSeq
    val deindented = lines.head +: lines.tail.map { line =>
      val leading = line.takeWhile(c => c == ' ' || c == '\t').length
      line.drop(leading min baseIndent)
    }
    deindented.map(_.stripTrailing())
  }

  def indent(lines: Seq[String], indent: String): Seq[String] =
    lines.map(l => if (l.isBlank) "" else indent + l)

  /** `a.b.c`, `this.a`, `a` */
  def isSimplePath(expr: ScExpression): Boolean = expr match {
    case ref: ScReferenceExpression => ref.qualifier.forall(isSimplePath)
    case _: ScThisReference => true
    case _ => false
  }

  /**
   * Whether a simple path contains a method call, e.g. `builder.getTokenText` or `list.size`
   * (a parameterless `def` or a Java getter called without parentheses).
   * Unresolved references are treated as calls.
   */
  def pathContainsCall(path: ScExpression): Boolean = path match {
    case ref: ScReferenceExpression =>
      ref.qualifier.exists(pathContainsCall) || (ref.resolve() match {
        case null | _: PsiMethod => true
        case _ => false
      })
    case _ => false
  }

  /**
   * Expressions which may serve as the subject of a `match` built from an `if` chain:
   * simple paths (`a.b.c`, `this.a`) and chains of method calls with simple arguments
   * (`builder.getTokenText`, `s.charAt(0)`, `xs.headOption.map(f)`, `foo[Int]()`).
   *
   * Assignments, instance creation, blocks, lambdas, control structures and operators are not accepted.
   */
  def isSubjectCandidate(expr: ScExpression): Boolean = stripParentheses(expr) match {
    case _: ScThisReference => true
    case ref: ScReferenceExpression => !ref.refName.endsWith("_=") && ref.qualifier.forall(isSubjectCandidate)
    case ScGenericCall(ref, _) => isSubjectCandidate(ref)
    case ScMethodCall(invoked, args) => isSubjectCandidate(invoked) && args.forall(isSimpleArgument)
    case _ => false
  }

  private def isSimpleArgument(arg: ScExpression): Boolean = arg match {
    case _: ScLiteral => true
    case _ => isSubjectCandidate(arg)
  }

  def stripParentheses(expr: ScExpression): ScExpression = expr match {
    case ScParenthesisedExpr(inner) => stripParentheses(inner)
    case _ => expr
  }

  /**
   * Structural equivalence, ignoring whitespace, comments and outer parentheses;
   * references must resolve to the same elements.
   */
  def sameExpression(a: ScExpression, b: ScExpression): Boolean =
    PsiEquivalenceUtil.areElementsEquivalent(stripParentheses(a), stripParentheses(b))
}
