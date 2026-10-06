package org.jetbrains.plugins.scala.codeInspection.assignment

import com.intellij.codeInspection.LocalInspectionTool
import com.intellij.profile.codeInspection.InspectionProfileManager
import org.jetbrains.plugins.scala.codeInspection.{ScalaInspectionBundle, ScalaInspectionTestBase}
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}

abstract class ScalaOperatorAssignmentInspectionTestBase extends ScalaInspectionTestBase {
  override protected val classOfInspection: Class[_ <: LocalInspectionTool] = classOf[ScalaOperatorAssignmentInspection]

  override protected val description: String = ScalaInspectionBundle.message("reassignment.can.be.simplified")

  protected def hint(replacement: String): String = ScalaInspectionBundle.message("replace.with.operator.assignment", replacement)

  protected def withoutRightAssociative(body: => Unit): Unit = {
    val tool = InspectionProfileManager.getInstance(getProject)
      .getCurrentProfile
      .getInspectionTool("ScalaOperatorAssignment", getProject)
      .getTool
      .asInstanceOf[ScalaOperatorAssignmentInspection]
    tool.setReportRightAssociative(false)
    try body
    finally tool.setReportRightAssociative(true)
  }
}

class ScalaOperatorAssignmentInspectionTest extends ScalaOperatorAssignmentInspectionTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  private def doTest(statement: String, expected: String, declaration: String = "var x = 0"): Unit = testQuickFix(
    s"""object Test {
       |  $declaration
       |  def foo(e: Int): Unit = {
       |    $START$statement$END
       |  }
       |}
       |""".stripMargin,
    s"""object Test {
       |  $declaration
       |  def foo(e: Int): Unit = {
       |    $expected
       |  }
       |}
       |""".stripMargin,
    hint(expected)
  )

  private def checkNoError(statement: String, declaration: String = "var x = 0"): Unit = checkTextHasNoErrors(
    s"""object Test {
       |  $declaration
       |  def foo(e: Int): Unit = {
       |    $statement
       |  }
       |}
       |""".stripMargin
  )

  def testPlus(): Unit = doTest("x = x + e", "x += e")
  def testMinus(): Unit = doTest("x = x - e", "x -= e")
  def testTimes(): Unit = doTest("x = x * e", "x *= e")
  def testDiv(): Unit = doTest("x = x / e", "x /= e")
  def testRem(): Unit = doTest("x = x % e", "x %= e")
  def testOr(): Unit = doTest("x = x | e", "x |= e")
  def testAnd(): Unit = doTest("x = x & e", "x &= e")
  def testXor(): Unit = doTest("x = x ^ e", "x ^= e")
  def testShift(): Unit = doTest("x = x << e", "x <<= e")
  def testHigherPrecedenceArgument(): Unit = doTest("x = x + e * 2", "x += e * 2")
  def testThisQualified(): Unit = doTest("this.x = x + e", "this.x += e")
  def testString(): Unit = doTest("""x = x + "a"""", """x += "a"""", declaration = """var x = """"")
  def testLocalVariable(): Unit = testQuickFix(
    s"""object Test {
       |  def foo(e: Int): Int = {
       |    var x = 0
       |    ${START}x = x + e$END
       |    x
       |  }
       |}
       |""".stripMargin,
    s"""object Test {
       |  def foo(e: Int): Int = {
       |    var x = 0
       |    x += e
       |    x
       |  }
       |}
       |""".stripMargin,
    hint("x += e")
  )

  def testVectorAppend(): Unit = doTest("xs = xs :+ e", "xs :+= e", declaration = "var xs = Vector.empty[Int]")

  // the left operand differs
  def testLowerPrecedenceOperand(): Unit = checkNoError("x = x * 2 + e")
  def testReversedOperands(): Unit = checkNoError("x = e + x")
  def testOtherVariable(): Unit = checkNoError("x = y + e", declaration = "var x = 0; var y = 0")
  def testComparison(): Unit = checkNoError("b = b == true", declaration = "var b = false")
  def testAlphanumericMethod(): Unit = checkNoError("x = x max e")
  def testVal(): Unit = checkNoError("x = x + e", declaration = "val x = 0")

  // `buf ++= ys` would mutate the buffer in place instead of creating a new one
  def testMutableCollection(): Unit = checkNoError(
    "buf = buf ++ Seq(e)",
    declaration = "var buf = scala.collection.mutable.ArrayBuffer.empty[Int]"
  )

  def testMutableCollectionViaVal(): Unit = checkNoError(
    "b2 = b2 ++ Seq(2)",
    declaration = "val buf = scala.collection.mutable.ArrayBuffer(1); var b2 = buf"
  )

  // the examples from SCL-8152
  private val seqDeclaration = "var seq = Seq(1, 2, 3)"
  private val listDeclaration = "var list = List(1, 2, 3)"
  private val mapDeclaration = """var map = Map(1 -> "foo")"""

  def testSeqPrepend(): Unit = doTest("seq = 0 +: seq", "seq +:= 0", declaration = seqDeclaration)
  def testSeqAppend(): Unit = doTest("seq = seq :+ 4", "seq :+= 4", declaration = seqDeclaration)
  def testSeqAppendAll(): Unit = doTest("seq = seq ++ Seq(4)", "seq ++= Seq(4)", declaration = seqDeclaration)
  def testSeqPrependAll(): Unit = doTest("seq = Seq(0) ++: seq", "seq ++:= Seq(0)", declaration = seqDeclaration)
  def testListCons(): Unit = doTest("list = 0 :: list", "list ::= 0", declaration = listDeclaration)
  def testListConcat(): Unit = doTest("list = List(0) ::: list", "list :::= List(0)", declaration = listDeclaration)
  def testListConsHigherPrecedenceArgument(): Unit = doTest("list = e + 1 :: list", "list ::= e + 1", declaration = listDeclaration)
  def testMapPlus(): Unit = doTest("""map = map + (2 -> "bar")""", """map += (2 -> "bar")""", declaration = mapDeclaration)
  def testMapUpdated(): Unit = doTest("""map = map.updated(2, "bar")""", """map += 2 -> "bar"""", declaration = mapDeclaration)
  def testMapUpdatedInfixKey(): Unit = doTest("""map = map.updated(e + 1, "bar")""", """map += (e + 1) -> "bar"""", declaration = mapDeclaration)
  def testMapConcat(): Unit = doTest("""map = map ++ Map(2 -> "bar")""", """map ++= Map(2 -> "bar")""", declaration = mapDeclaration)
  def testMapMinus(): Unit = doTest("map = map - 1", "map -= 1", declaration = mapDeclaration)
  def testMapMinusAll(): Unit = doTest("map = map -- Seq(1)", "map --= Seq(1)", declaration = mapDeclaration)
  def testSetPlus(): Unit = doTest("xs = xs + e", "xs += e", declaration = "var xs = Set.empty[Int]")

  // `Seq(0) ++ seq` calls `++` on `Seq(0)`, while `seq ++:= Seq(0)` calls `++:` on `seq`
  def testSeqConcatToTheLeft(): Unit = checkNoError("seq = Seq(0) ++ seq", declaration = seqDeclaration)
  // the right map wins on duplicate keys, so the operands cannot be swapped
  def testMapConcatToTheLeft(): Unit = checkNoError("""map = Map(2 -> "bar") ++ map""", declaration = mapDeclaration)
  // the right operand of a right-associative operator differs
  def testListConsOfOtherList(): Unit = checkNoError("list = 0 :: e :: list", declaration = listDeclaration)
  def testMutableMapUpdated(): Unit = checkNoError(
    """map = map.updated(2, "bar")""",
    declaration = """var map = scala.collection.mutable.Map(1 -> "foo")"""
  )
  def testSeqUpdated(): Unit = checkNoError("seq = seq.updated(0, e)", declaration = seqDeclaration)
  def testMapUpdatedOfOtherMap(): Unit = checkNoError(
    """map = other.updated(2, "bar")""",
    declaration = """var map = Map(1 -> "foo"); val other = Map(1 -> "foo")"""
  )

  def testRightAssociativeDisabled(): Unit = withoutRightAssociative {
    checkNoError("list = 0 :: list", declaration = listDeclaration)
  }

  def testLeftAssociativeWithRightAssociativeDisabled(): Unit = withoutRightAssociative {
    doTest("seq = seq :+ 4", "seq :+= 4", declaration = seqDeclaration)
  }

  def testNamedArgument(): Unit = checkTextHasNoErrors(
    """object Test {
      |  def bar(x: Int): Int = x
      |  def foo(x: Int): Int = bar(x = x + 1)
      |}
      |""".stripMargin
  )
}

class ScalaOperatorAssignmentInspectionTest_Scala3 extends ScalaOperatorAssignmentInspectionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3

  // `buf ++= ys` would mutate the buffer in place instead of creating a new one
  def testMutableCollection(): Unit = checkTextHasNoErrors(
    """object Test:
      |  val buf = scala.collection.mutable.ArrayBuffer(1)
      |  var b2 = buf
      |  b2 = b2 ++ Seq(2)
      |""".stripMargin
  )

  def testPlus(): Unit = testQuickFix(
    s"""object Test:
       |  var x = 0
       |  def foo(e: Int): Unit =
       |    ${START}x = x + e$END
       |""".stripMargin,
    s"""object Test:
       |  var x = 0
       |  def foo(e: Int): Unit =
       |    x += e
       |""".stripMargin,
    hint("x += e")
  )

  def testCons(): Unit = testQuickFix(
    s"""object Test:
       |  var list = List(1, 2, 3)
       |  def foo(e: Int): Unit =
       |    ${START}list = e :: list$END
       |""".stripMargin,
    s"""object Test:
       |  var list = List(1, 2, 3)
       |  def foo(e: Int): Unit =
       |    list ::= e
       |""".stripMargin,
    hint("list ::= e")
  )
}
