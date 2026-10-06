package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScCaseClause
import org.jetbrains.plugins.scala.lang.psi.api.expr._

/**
 * Utilities shared by [[org.jetbrains.plugins.scala.codeInspection.controlFlow.ScalaLiftReturnOrAssignmentInspection]]
 * and [[UnfoldReturnOrAssignmentIntention]].
 *
 * A "branching expression" is an `if` with an `else` branch, a `match` or a `try` (with optional `catch` cases).
 * Its "tails" are the expressions that produce the value of each branch,
 * i.e. the last expressions of the branches, with nested branching expressions and blocks flattened.
 */
private[scala] object BranchTails {

  def isBranching(expr: ScExpression): Boolean = expr match {
    case ScIf(_, Some(_), Some(_)) => true
    case _: ScMatch | _: ScTry => true
    case _ => false
  }

  /** @return `None` if some branch doesn't have a tail expression (e.g. `if` without `else`, empty block, `catch handler`) */
  def collect(expr: ScExpression): Option[Seq[ScExpression]] = expr match {
    case block: ScBlockExpr if block.hasCaseClauses => None
    case block: ScBlock => block.resultExpression.flatMap(collect)
    case ScIf(_, Some(thenBranch), Some(elseBranch)) =>
      for {
        thenTails <- collect(thenBranch)
        elseTails <- collect(elseBranch)
      } yield thenTails ++ elseTails
    case _: ScIf => None
    case m: ScMatch => collectClauses(m.clauses)
    case t: ScTry =>
      for {
        tryTails <- t.expression.flatMap(collect)
        catchTails <- t.catchBlock match {
          case None => Some(Seq.empty)
          case Some(catchBlock) => catchClauses(catchBlock).flatMap(collectClauses)
        }
      } yield tryTails ++ catchTails
    case other => Some(Seq(other))
  }

  def keywordText(expr: ScExpression): String = expr match {
    case _: ScIf => "if"
    case _: ScMatch => "match"
    case _: ScTry => "try"
    case _ => ""
  }

  /** Tails which have type `Nothing` regardless of the context and therefore must be left untouched */
  def isJump(tail: ScExpression): Boolean = tail.is[ScThrow, ScReturn]

  /**
   * Only simple targets (`x`, `this.x`, `a.b.x`) are supported:
   * moving an assignment to e.g. `arr(i()) = ...` across a condition would change the evaluation order.
   */
  def isSimpleAssignmentTarget(expr: ScExpression): Boolean = expr match {
    case ref: ScReferenceExpression => ref.qualifier.forall(q => q.is[ScThisReference] || isSimpleAssignmentTarget(q))
    case _ => false
  }

  private def catchClauses(catchBlock: ScCatchBlock): Option[Seq[ScCaseClause]] =
    catchBlock.caseClauses.orElse {
      catchBlock.expression.collect { case block: ScBlockExpr if block.hasCaseClauses => block.caseClauses }.flatten
    }.map(_.caseClauses).orElse {
      // Scala 3 `catch case e => ...` without braces: the clause is a direct child of the catch block
      catchBlock.getChildren.collectFirst { case clause: ScCaseClause => Seq(clause) }
    }

  private def collectClauses(clauses: Seq[ScCaseClause]): Option[Seq[ScExpression]] =
    if (clauses.isEmpty) None
    else clauses.foldLeft(Option(Seq.empty[ScExpression])) { (acc, clause) =>
      for {
        tails <- acc
        body <- clause.expr
        clauseTails <- collect(body)
      } yield tails ++ clauseTails
    }
}
