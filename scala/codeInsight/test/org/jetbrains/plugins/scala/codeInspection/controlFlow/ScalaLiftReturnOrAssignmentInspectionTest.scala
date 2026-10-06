package org.jetbrains.plugins.scala.codeInspection.controlFlow

import com.intellij.codeInspection.LocalInspectionTool
import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.profile.codeInspection.InspectionProjectProfileManager
import org.jetbrains.plugins.scala.codeInspection.ScalaInspectionTestBase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.junit.Assert.assertEquals

abstract class ScalaLiftReturnOrAssignmentInspectionTestBase extends ScalaInspectionTestBase {

  override protected val classOfInspection: Class[_ <: LocalInspectionTool] = classOf[ScalaLiftReturnOrAssignmentInspection]

  override protected val description: String = "Return or assignment can be lifted out of 'if', 'match' or 'try'"

  override protected def descriptionMatches(s: String): Boolean =
    s != null && s.matches("(Return|Assignment) can be lifted out of '(if|match|try)'")

  protected val Weak: HighlightSeverity = HighlightSeverity.WEAK_WARNING
  protected val Info: HighlightSeverity = HighlightSeverity.INFORMATION

  /**
   * @param text     the keyword of the reported expression must be surrounded by `START`/`END` markers
   * @param severity the expected severity of the (single) problem
   */
  protected def doTest(text: String, expected: String, hint: String, severity: HighlightSeverity): Unit = {
    checkTextHasError(text)
    assertSeverity(text, severity)
    testQuickFix(text, expected, hint)
  }

  protected def assertSeverity(text: String, severity: HighlightSeverity): Unit = {
    configureByText(text)
    assertEquals(Seq(severity), findMatchingHighlightings(text).map(_.getSeverity))
  }

  protected def withOption(reportOnlyIfSingleStatement: Boolean)(body: => Unit): Unit = {
    val profile = InspectionProjectProfileManager.getInstance(getProject).getCurrentProfile
    val tool = profile.getInspectionTool("ScalaLiftReturnOrAssignment", getProject).getTool
      .asInstanceOf[ScalaLiftReturnOrAssignmentInspection]
    val oldValue = tool.reportOnlyIfSingleStatement
    tool.reportOnlyIfSingleStatement = reportOnlyIfSingleStatement
    try body
    finally tool.reportOnlyIfSingleStatement = oldValue
  }
}

class ScalaLiftReturnOrAssignmentInspectionTest extends ScalaLiftReturnOrAssignmentInspectionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  def testLiftReturnOutOfIf(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    ${START}if$END (i == 0) return "zero" else return "other"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    return if (i == 0) "zero" else "other"
       |  }
       |}""".stripMargin,
    "Lift return out of 'if'",
    Weak
  )

  def testLiftAssignmentOutOfIf(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    ${START}if$END (i == 0) x = "zero" else x = "other"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    x = if (i == 0) "zero" else "other"
       |  }
       |}""".stripMargin,
    "Lift assignment out of 'if'",
    Weak
  )

  def testLiftReturnMultiStatementBranches(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    ${START}if$END (i == 0) {
       |      println("zero")
       |      return "zero"
       |    } else {
       |      println("other")
       |      return "other"
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    return if (i == 0) {
       |      println("zero")
       |      "zero"
       |    } else {
       |      println("other")
       |      "other"
       |    }
       |  }
       |}""".stripMargin,
    "Lift return out of 'if'",
    Info
  )

  def testLiftReturnMultiStatementBranchesWithOptionDisabled(): Unit = withOption(reportOnlyIfSingleStatement = false) {
    assertSeverity(
      s"""class X {
         |  def f(i: Int): String = {
         |    ${START}if$END (i == 0) {
         |      println("zero")
         |      return "zero"
         |    } else return "other"
         |  }
         |}""".stripMargin,
      Weak
    )
  }

  def testLiftReturnElseIfChain(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    ${START}if$END (i == 0) return "zero"
       |    else if (i > 0) return "positive"
       |    else return "negative"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    return if (i == 0) "zero"
       |    else if (i > 0) "positive"
       |    else "negative"
       |  }
       |}""".stripMargin,
    "Lift return out of 'if'",
    Weak
  )

  def testLiftReturnOutOfMatch(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    i ${START}match$END {
       |      case 0 => return "zero"
       |      case 1 => return "one"
       |      case _ => throw new IllegalArgumentException
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    return i match {
       |      case 0 => "zero"
       |      case 1 => "one"
       |      case _ => throw new IllegalArgumentException
       |    }
       |  }
       |}""".stripMargin,
    "Lift return out of 'match'",
    Weak
  )

  def testLiftReturnOutOfMatchMultiStatementClause(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    i ${START}match$END {
       |      case 0 => return "zero"
       |      case 1 =>
       |        println("one")
       |        return "one"
       |      case _ => throw new IllegalArgumentException
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    return i match {
       |      case 0 => "zero"
       |      case 1 =>
       |        println("one")
       |        "one"
       |      case _ => throw new IllegalArgumentException
       |    }
       |  }
       |}""".stripMargin,
    "Lift return out of 'match'",
    Info
  )

  def testLiftAssignmentOutOfMatch(): Unit = doTest(
    s"""class X {
       |  var x = ""
       |
       |  def f(i: Int): Unit = {
       |    i ${START}match$END {
       |      case 0 => this.x = "zero"
       |      case _ => this.x = "other"
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  var x = ""
       |
       |  def f(i: Int): Unit = {
       |    this.x = i match {
       |      case 0 => "zero"
       |      case _ => "other"
       |    }
       |  }
       |}""".stripMargin,
    "Lift assignment out of 'match'",
    Weak
  )

  def testLiftReturnOutOfTry(): Unit = doTest(
    s"""class X {
       |  def f(s: String): Int = {
       |    ${START}try$END {
       |      return s.toInt
       |    } catch {
       |      case _: NumberFormatException => return 0
       |    } finally {
       |      println("done")
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(s: String): Int = {
       |    return try {
       |      s.toInt
       |    } catch {
       |      case _: NumberFormatException => 0
       |    } finally {
       |      println("done")
       |    }
       |  }
       |}""".stripMargin,
    "Lift return out of 'try'",
    Weak
  )

  def testLiftReturnOutOfTrySingleLiftableBranch(): Unit = doTest(
    s"""class X {
       |  def f(s: String): Int = {
       |    ${START}try$END {
       |      return s.toInt
       |    } catch {
       |      case e: NumberFormatException => throw new IllegalStateException(e)
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(s: String): Int = {
       |    return try {
       |      s.toInt
       |    } catch {
       |      case e: NumberFormatException => throw new IllegalStateException(e)
       |    }
       |  }
       |}""".stripMargin,
    "Lift return out of 'try'",
    Info
  )

  def testLiftNestedIfInBranch(): Unit = doTest(
    s"""class X {
       |  def f(a: Boolean, b: Boolean): Int = {
       |    ${START}if$END (a) {
       |      if (b) return 1 else return 2
       |    } else return 3
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(a: Boolean, b: Boolean): Int = {
       |    return if (a) {
       |      if (b) 1 else 2
       |    } else 3
       |  }
       |}""".stripMargin,
    "Lift return out of 'if'",
    Weak
  )

  def testOtherReturnsInside(): Unit = assertSeverity(
    s"""class X {
       |  def f(i: Int, xs: List[Int]): Int = {
       |    ${START}if$END (i == 0) return xs.map(x => if (x < 0) return x else x).sum
       |    else return 1
       |  }
       |}""".stripMargin,
    Info
  )

  def testLongExpression(): Unit = assertSeverity(
    s"""class X {
       |  def f(i: Int): Int = {
       |    i ${START}match$END {
       |      case 0 => return 0
       |      case 1 => return 1
       |      case 2 => return 2
       |      case 3 => return 3
       |      case 4 => return 4
       |      case 5 => return 5
       |      case 6 => return 6
       |      case 7 => return 7
       |      case 8 => return 8
       |      case 9 => return 9
       |      case 10 => return 10
       |      case 11 => return 11
       |      case 12 => return 12
       |      case 13 => return 13
       |      case _ => return -1
       |    }
       |  }
       |}""".stripMargin,
    Info
  )

  def testNotReportedWithoutElse(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int): String = {
       |    if (i == 0) return "zero"
       |    "other"
       |  }
       |}""".stripMargin
  )

  def testNotReportedWhenValueIsUsed(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int): String = {
       |    val s: String = if (i == 0) return "zero" else return "other"
       |    s
       |  }
       |}""".stripMargin
  )

  def testNotReportedForMixedReturnAndAssignment(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int): String = {
       |    var x = ""
       |    if (i == 0) return "zero" else x = "other"
       |    x
       |  }
       |}""".stripMargin
  )

  def testNotReportedForDifferentTargets(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    var y = ""
       |    if (i == 0) x = "zero" else y = "other"
       |  }
       |}""".stripMargin
  )

  def testNotReportedWhenBranchDoesNotEndWithReturn(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    if (i == 0) return else println("other")
       |  }
       |}""".stripMargin
  )

  def testNotReportedForUpdateCall(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int, arr: Array[Int]): Unit = {
       |    if (i == 0) arr(i) = 1 else arr(i) = 2
       |  }
       |}""".stripMargin
  )

  def testNotReportedForOnlyThrows(): Unit = checkTextHasNoErrors(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    if (i == 0) throw new Exception else throw new RuntimeException
       |  }
       |}""".stripMargin
  )
}

class ScalaLiftReturnOrAssignmentInspectionTest_Scala3 extends ScalaLiftReturnOrAssignmentInspectionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3

  override protected def setUp(): Unit = {
    super.setUp()
    getScalaCodeStyleSettings.USE_SCALA3_INDENTATION_BASED_SYNTAX = true
  }

  def testLiftReturnOutOfIf(): Unit = doTest(
    s"""def f(i: Int): String =
       |  ${START}if$END i == 0 then
       |    println("zero")
       |    return "zero"
       |  else
       |    return "other"
       |""".stripMargin,
    s"""def f(i: Int): String =
       |  return if i == 0 then
       |    println("zero")
       |    "zero"
       |  else
       |    "other"
       |""".stripMargin,
    "Lift return out of 'if'",
    Info
  )

  def testLiftAssignmentOutOfMatch(): Unit = doTest(
    s"""def f(i: Int): Unit =
       |  var x = ""
       |  i ${START}match$END
       |    case 0 => x = "zero"
       |    case _ => x = "other"
       |""".stripMargin,
    s"""def f(i: Int): Unit =
       |  var x = ""
       |  x = i match
       |    case 0 => "zero"
       |    case _ => "other"
       |""".stripMargin,
    "Lift assignment out of 'match'",
    Weak
  )

  def testLiftAssignmentOutOfBracelessTry(): Unit = doTest(
    s"""def f(s: String): Int =
       |  var v = 0
       |  ${START}try$END v = s.toInt
       |  catch case _: NumberFormatException => v = -1
       |  v
       |""".stripMargin,
    s"""def f(s: String): Int =
       |  var v = 0
       |  v = try s.toInt
       |  catch case _: NumberFormatException => -1
       |  v
       |""".stripMargin,
    "Lift assignment out of 'try'",
    Weak
  )
}
