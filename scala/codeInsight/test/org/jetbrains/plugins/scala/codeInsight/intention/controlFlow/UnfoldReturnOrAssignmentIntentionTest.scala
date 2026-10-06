package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import com.intellij.testFramework.TestIndexingModeSupporter.IndexingMode
import org.jetbrains.plugins.scala.util.runners.WithIndexingMode

@WithIndexingMode(mode = IndexingMode.DUMB_EMPTY_INDEX)
class UnfoldReturnOrAssignmentIntentionTest extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= ScalaVersion.Latest.Scala_2_13

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.unfold.return.or.assignment")

  def testUnfoldReturnToIf(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    ret${CARET}urn if (i == 0) "zero" else "other"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    if (i == 0) return "zero" else return "other"
       |  }
       |}""".stripMargin,
    expectedIntentionText = Some("Replace return with 'if' expression")
  )

  def testUnfoldAssignmentToIf(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    x $CARET= if (i == 0) "zero" else "other"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    if (i == 0) x = "zero" else x = "other"
       |  }
       |}""".stripMargin,
    expectedIntentionText = Some("Replace assignment with 'if' expression")
  )

  def testUnfoldAssignmentToElseIfChainWithBlocks(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    ${CARET}x = if (i == 0) {
       |      println("zero")
       |      "zero"
       |    } else if (i > 0) "positive"
       |    else "negative"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x = ""
       |    if (i == 0) {
       |      println("zero")
       |      x = "zero"
       |    } else if (i > 0) x = "positive"
       |    else x = "negative"
       |  }
       |}""".stripMargin
  )

  def testUnfoldReturnToMatch(): Unit = doTest(
    s"""class X {
       |  def f(i: Int): String = {
       |    ret${CARET}urn i match {
       |      case 0 => "zero"
       |      case _ => throw new IllegalArgumentException
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(i: Int): String = {
       |    i match {
       |      case 0 => return "zero"
       |      case _ => throw new IllegalArgumentException
       |    }
       |  }
       |}""".stripMargin,
    expectedIntentionText = Some("Replace return with 'match' expression")
  )

  def testUnfoldFieldAssignmentToMatch(): Unit = doTest(
    s"""class X {
       |  var x = ""
       |
       |  def f(i: Int): Unit = {
       |    this.${CARET}x = i match {
       |      case 0 => "zero"
       |      case _ => "other"
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  var x = ""
       |
       |  def f(i: Int): Unit = {
       |    i match {
       |      case 0 => this.x = "zero"
       |      case _ => this.x = "other"
       |    }
       |  }
       |}""".stripMargin
  )

  def testUnfoldReturnToTry(): Unit = doTest(
    s"""class X {
       |  def f(s: String): Int = {
       |    ret${CARET}urn try s.toInt catch {
       |      case _: NumberFormatException => 0
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(s: String): Int = {
       |    try return s.toInt catch {
       |      case _: NumberFormatException => return 0
       |    }
       |  }
       |}""".stripMargin
  )

  def testNotAvailableForVal(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    val ${CARET}x = if (i == 0) "zero" else "other"
       |  }
       |}""".stripMargin
  )

  def testNotAvailableForVarDefinition(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var ${CARET}x = if (i == 0) "zero" else "other"
       |  }
       |}""".stripMargin
  )

  def testNotAvailableForIfWithoutElse(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(i: Int): Unit = {
       |    var x: Any = ()
       |    ${CARET}x = if (i == 0) "zero"
       |  }
       |}""".stripMargin
  )

  def testNotAvailableForSimpleValue(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(i: Int): Int = {
       |    ret${CARET}urn i + 1
       |  }
       |}""".stripMargin
  )

  def testNotAvailableInsideValue(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(i: Int): String = {
       |    return if (i == 0) "ze${CARET}ro" else "other"
       |  }
       |}""".stripMargin
  )

  def testNotAvailableForNamedArgument(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def g(x: String): Unit = ()
       |  def f(i: Int): Unit = {
       |    g(${CARET}x = if (i == 0) "zero" else "other")
       |  }
       |}""".stripMargin
  )
}

@WithIndexingMode(mode = IndexingMode.DUMB_EMPTY_INDEX)
class UnfoldReturnOrAssignmentIntentionTest_Scala3 extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_3

  override protected def setUp(): Unit = {
    super.setUp()
    getScalaCodeStyleSettings.USE_SCALA3_INDENTATION_BASED_SYNTAX = true
  }

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.unfold.return.or.assignment")

  def testUnfoldReturnToIf(): Unit = doTest(
    s"""def f(i: Int): String =
       |  ret${CARET}urn if i == 0 then
       |    println("zero")
       |    "zero"
       |  else
       |    "other"
       |""".stripMargin,
    s"""def f(i: Int): String =
       |  if i == 0 then
       |    println("zero")
       |    return "zero"
       |  else
       |    return "other"
       |""".stripMargin
  )

  def testUnfoldAssignmentToMatch(): Unit = doTest(
    s"""def f(i: Int): Unit =
       |  var x = ""
       |  ${CARET}x = i match
       |    case 0 => "zero"
       |    case _ =>
       |      println("other")
       |      "other"
       |""".stripMargin,
    s"""def f(i: Int): Unit =
       |  var x = ""
       |  i match
       |    case 0 => x = "zero"
       |    case _ =>
       |      println("other")
       |      x = "other"
       |""".stripMargin
  )

  def testUnfoldAssignmentToBracelessTry(): Unit = doTest(
    s"""def f(s: String): Int =
       |  var v = 0
       |  v ${CARET}= try s.toInt
       |  catch case _: NumberFormatException => -1
       |  v
       |""".stripMargin,
    s"""def f(s: String): Int =
       |  var v = 0
       |  try v = s.toInt
       |  catch case _: NumberFormatException => v = -1
       |  v
       |""".stripMargin
  )
}
