package org.jetbrains.plugins.scala.codeInsight.intention.matcher

import org.jetbrains.plugins.scala.codeInsight.intentions.ScalaIntentionTestBase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

abstract class AddRemainingCasesIntentionTestBase extends ScalaIntentionTestBase {

  override def familyName: String = "Add remaining cases"

  protected val Shapes: String =
    """sealed trait Shape
      |
      |case class Circle(radius: Double) extends Shape
      |
      |case class Rect(w: Double, h: Double) extends Shape
      |
      |case object Empty extends Shape
      |
      |""".stripMargin
}

final class AddRemainingCasesIntentionTest extends AddRemainingCasesIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  def testSealedTraitPartiallyCovered(): Unit = doTest(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case Circle(r) => r * r
       |}
       |""".stripMargin,
    s"""$Shapes
       |def area(s: Shape): Double = s match {
       |  case Circle(r) => r * r
       |  case Rect(w, h) => <selection>$CARET???</selection>
       |  case Empty => ???
       |}
       |""".stripMargin
  )

  def testInsertBeforeTrailingWildcard(): Unit = doTest(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case Empty => 0
       |  case _ => 1
       |}
       |""".stripMargin,
    s"""$Shapes
       |def area(s: Shape): Double = s match {
       |  case Empty => 0
       |  case Circle(radius) => ???
       |  case Rect(w, h) => ???
       |  case _ => 1
       |}
       |""".stripMargin
  )

  def testTypedPatternCoversClass(): Unit = doTest(
    s"""$Shapes
       |def area(s: Shape): Double = ${CARET}s match {
       |  case c: Circle => 1
       |  case Empty => 0
       |}
       |""".stripMargin,
    s"""$Shapes
       |def area(s: Shape): Double = s match {
       |  case c: Circle => 1
       |  case Empty => 0
       |  case Rect(w, h) => ???
       |}
       |""".stripMargin
  )

  def testGuardedClauseDoesNotCover(): Unit = doTest(
    s"""$Shapes
       |def area(s: Shape, flag: Boolean): Double = s ${CARET}match {
       |  case Circle(r) => 1
       |  case Rect(w, h) => 2
       |  case Empty if flag => 0
       |}
       |""".stripMargin,
    s"""$Shapes
       |def area(s: Shape, flag: Boolean): Double = s match {
       |  case Circle(r) => 1
       |  case Rect(w, h) => 2
       |  case Empty if flag => 0
       |  case Empty => ???
       |}
       |""".stripMargin
  )

  def testRefutableConstructorPatternDoesNotCover(): Unit = doTest(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case Circle(0) => 0
       |  case Rect(w, h) => 2
       |  case Empty => 0
       |}
       |""".stripMargin,
    s"""$Shapes
       |def area(s: Shape): Double = s match {
       |  case Circle(0) => 0
       |  case Rect(w, h) => 2
       |  case Empty => 0
       |  case Circle(radius) => ???
       |}
       |""".stripMargin
  )

  def testOption(): Unit = doTest(
    s"""def f(o: Option[Int]): Int = o ${CARET}match {
       |  case Some(x) => x
       |}
       |""".stripMargin,
    s"""def f(o: Option[Int]): Int = o match {
       |  case Some(x) => x
       |  case None => ???
       |}
       |""".stripMargin
  )

  def testAlternativesCover(): Unit = checkIntentionIsNotAvailable(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case Circle(_) | Rect(_, _) => 1
       |  case Empty => 0
       |}
       |""".stripMargin
  )

  def testAlreadyExhaustive(): Unit = checkIntentionIsNotAvailable(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case Circle(r) => 1
       |  case Rect(w, h) => 2
       |  case Empty => 0
       |}
       |""".stripMargin
  )

  def testSupertypePatternCoversEverything(): Unit = checkIntentionIsNotAvailable(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case Empty => 0
       |  case other: Shape => 1
       |}
       |""".stripMargin
  )

  def testWildcardInTheMiddleCoversEverything(): Unit = checkIntentionIsNotAvailable(
    s"""$Shapes
       |def area(s: Shape): Double = s ${CARET}match {
       |  case x => 0
       |  case Empty => 1
       |}
       |""".stripMargin
  )

  def testNotAvailableInCaseBody(): Unit = checkIntentionIsNotAvailable(
    s"""$Shapes
       |def area(s: Shape): Double = s match {
       |  case Circle(r) => r$CARET * r
       |}
       |""".stripMargin
  )

  def testNotAvailableForNonSealedType(): Unit = checkIntentionIsNotAvailable(
    s"""def f(i: Int): Int = i ${CARET}match {
       |  case 1 => 1
       |}
       |""".stripMargin
  )
}

final class AddRemainingCasesIntentionTest_Scala3 extends AddRemainingCasesIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3_0

  def testEnumIndentationBased(): Unit = doTest(
    s"""enum Color:
       |  case Red, Green
       |  case Rgb(r: Int, g: Int, b: Int)
       |
       |def name(c: Color): String = c ${CARET}match
       |  case Color.Red => "red"
       |""".stripMargin,
    s"""enum Color:
       |  case Red, Green
       |  case Rgb(r: Int, g: Int, b: Int)
       |
       |def name(c: Color): String = c match
       |  case Color.Red => "red"
       |  case Color.Green => ???
       |  case Color.Rgb(r, g, b) => ???
       |""".stripMargin
  )

  def testEnumBeforeWildcardIndentationBased(): Unit = doTest(
    s"""enum Color:
       |  case Red, Green, Blue
       |
       |def name(c: Color): String = c ${CARET}match
       |  case Color.Green => "green"
       |  case _ => "other"
       |""".stripMargin,
    s"""enum Color:
       |  case Red, Green, Blue
       |
       |def name(c: Color): String = c match
       |  case Color.Green => "green"
       |  case Color.Red => ???
       |  case Color.Blue => ???
       |  case _ => "other"
       |""".stripMargin
  )
}
