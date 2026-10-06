package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

import com.intellij.testFramework.TestIndexingModeSupporter.IndexingMode
import org.jetbrains.plugins.scala.util.runners.WithIndexingMode

@WithIndexingMode(mode = IndexingMode.DUMB_EMPTY_INDEX)
class MatchToIfIntentionTest extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.match.to.if")

  def testLiterals(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): String =
       |    n mat${CARET}ch {
       |      case 1 => "one"
       |      case 2 | 3 => "two or three"
       |      case -1 => "minus one"
       |      case _ => "many"
       |    }
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): String =
       |    if (n == 1) "one"
       |    else if (n == 2 || n == 3) "two or three"
       |    else if (n == -1) "minus one"
       |    else "many"
       |}""".stripMargin
  )

  def testBlocksAndComments(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): Unit = {
       |    var res = ""
       |    ${CARET}n match {
       |      case 1 =>
       |        res = "one"
       |        println(res)
       |      case 2 => res = "two"
       |      case _ =>
       |        // unknown
       |        res = "???"
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): Unit = {
       |    var res = ""
       |    if (n == 1) {
       |      res = "one"
       |      println(res)
       |    } else if (n == 2) {
       |      res = "two"
       |    } else {
       |      // unknown
       |      res = "???"
       |    }
       |  }
       |}""".stripMargin
  )

  def testStableIdentifiersTypesAndGuards(): Unit = doTest(
    s"""object Colors {
       |  val Red = 1
       |}
       |
       |class X {
       |  def f(a: Any, zero: Int, flag: Boolean): String =
       |    a ${CARET}match {
       |      case Colors.Red => "red"
       |      case `zero` if flag || zero > 1 => "zero"
       |      case null => "null"
       |      case _: String | _: Int if flag => "string or int"
       |      case _ if !flag => "not flag"
       |      case _ => "other"
       |    }
       |}""".stripMargin,
    s"""object Colors {
       |  val Red = 1
       |}
       |
       |class X {
       |  def f(a: Any, zero: Int, flag: Boolean): String =
       |    if (a == Colors.Red) "red"
       |    else if (a == zero && (flag || zero > 1)) "zero"
       |    else if (a == null) "null"
       |    else if ((a.isInstanceOf[String] || a.isInstanceOf[Int]) && flag) "string or int"
       |    else if (!flag) "not flag"
       |    else "other"
       |}""".stripMargin
  )

  def testNoDefaultInStatementPosition(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): Unit = {
       |    n ${CARET}match {
       |      case 1 => println("one")
       |      case 2 => println("two")
       |    }
       |    println()
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): Unit = {
       |    if (n == 1) println("one")
       |    else if (n == 2) println("two")
       |    println()
       |  }
       |}""".stripMargin
  )

  def testNotAvailableNoDefaultUsedAsValue(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(n: Int): String =
       |    n ${CARET}match {
       |      case 1 => "one"
       |      case 2 => "two"
       |    }
       |}""".stripMargin
  )

  def testNotAvailableWithBinder(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(a: Any): String =
       |    a ${CARET}match {
       |      case s: String => s
       |      case _ => "other"
       |    }
       |}""".stripMargin
  )

  def testNotAvailableWithConstructorPattern(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(a: Option[Int]): Int =
       |    a ${CARET}match {
       |      case Some(1) => 1
       |      case _ => 0
       |    }
       |}""".stripMargin
  )

  def testNotAvailableComplexScrutinee(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def g(): Int = 1
       |  def f(): String =
       |    g() ${CARET}match {
       |      case 1 => "one"
       |      case _ => "other"
       |    }
       |}""".stripMargin
  )

  def testNotAvailableInCaseBody(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(n: Int): String =
       |    n match {
       |      case 1 => "o${CARET}ne"
       |      case _ => "other"
       |    }
       |}""".stripMargin
  )
}

// scrutinees with method calls can be recognized only in smart mode
class MatchToIfIntentionSmartModeTest extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.match.to.if")

  def testValuePathScrutineeIsRepeated(): Unit = doTest(
    s"""case class Point(x: Int, y: Int)
       |
       |class X {
       |  def f(p: Point): String =
       |    p.x ${CARET}match {
       |      case 0 => "zero"
       |      case _ => "other"
       |    }
       |}""".stripMargin,
    s"""case class Point(x: Int, y: Int)
       |
       |class X {
       |  def f(p: Point): String =
       |    if (p.x == 0) "zero"
       |    else "other"
       |}""".stripMargin
  )

  // SCL-16885
  def testCallScrutineeIsExtractedToValue(): Unit = doTest(
    s"""class Builder {
       |  def getTokenText: String = ""
       |}
       |
       |class X {
       |  def f(builder: Builder): Int = {
       |    builder.getTokenText ${CARET}match {
       |      case "1" | "2" => 1
       |      case "3" => 3
       |      case _ => 0
       |    }
       |  }
       |}""".stripMargin,
    s"""class Builder {
       |  def getTokenText: String = ""
       |}
       |
       |class X {
       |  def f(builder: Builder): Int = {
       |    val text = builder.getTokenText
       |    if (text == "1" || text == "2") 1
       |    else if (text == "3") 3
       |    else 0
       |  }
       |}""".stripMargin
  )

  def testCallScrutineeWithArgumentsIsExtractedToValue(): Unit = doTest(
    s"""class X {
       |  def f(s: String): Unit = {
       |    println(s)
       |    s.charAt(0) ${CARET}match {
       |      case 'a' => println("a")
       |      case 'b' => println("b")
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(s: String): Unit = {
       |    println(s)
       |    val c = s.charAt(0)
       |    if (c == 'a') println("a")
       |    else if (c == 'b') println("b")
       |  }
       |}""".stripMargin
  )

  def testCallScrutineeAsMethodBodyIsWrappedInBlock(): Unit = doTest(
    s"""class Builder {
       |  def getTokenText: String = ""
       |}
       |
       |class X {
       |  def back(builder: Builder): Unit =
       |    builder.getTokenText ${CARET}match {
       |      case "a" => println("A")
       |      case "b" | "c" => println("BC")
       |      case _ => println("other")
       |    }
       |}""".stripMargin,
    s"""class Builder {
       |  def getTokenText: String = ""
       |}
       |
       |class X {
       |  def back(builder: Builder): Unit = {
       |    val text = builder.getTokenText
       |    if (text == "a") println("A")
       |    else if (text == "b" || text == "c") println("BC")
       |    else println("other")
       |  }
       |}""".stripMargin
  )

  def testCallScrutineeAsValBodyIsWrappedInBlock(): Unit = doTest(
    s"""class X {
       |  def g(): Int = 1
       |
       |  val r = g() ${CARET}match {
       |    case 1 => "one"
       |    case _ => "other"
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def g(): Int = 1
       |
       |  val r = {
       |    val i = g()
       |    if (i == 1) "one"
       |    else "other"
       |  }
       |}""".stripMargin
  )

  def testCallScrutineeAsArgumentIsWrappedInBlock(): Unit = doTest(
    s"""class X {
       |  def g(): Int = 1
       |
       |  def f(): Unit = println(g() ${CARET}match {
       |    case 1 => "one"
       |    case _ => "other"
       |  })
       |}""".stripMargin,
    s"""class X {
       |  def g(): Int = 1
       |
       |  def f(): Unit = println({
       |    val i = g()
       |    if (i == 1) "one"
       |    else "other"
       |  })
       |}""".stripMargin
  )
}

@WithIndexingMode(mode = IndexingMode.DUMB_EMPTY_INDEX)
class MatchToIfIntentionTest_Scala3 extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_3

  override protected def setUp(): Unit = {
    super.setUp()
    getScalaCodeStyleSettings.USE_SCALA3_INDENTATION_BASED_SYNTAX = true
  }

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.match.to.if")

  def testIndentationSyntax(): Unit = doTest(
    s"""def f(n: Int): String =
       |  n ${CARET}match
       |    case 1 =>
       |      val s = "one"
       |      s
       |    case 2 | 3 => "two or three"
       |    case _ => "many"
       |""".stripMargin,
    s"""def f(n: Int): String =
       |  if n == 1 then
       |    val s = "one"
       |    s
       |  else if n == 2 || n == 3 then
       |    "two or three"
       |  else
       |    "many"
       |""".stripMargin
  )
}

// scrutinees with method calls can be recognized only in smart mode
class MatchToIfIntentionSmartModeTest_Scala3 extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_3

  override protected def setUp(): Unit = {
    super.setUp()
    getScalaCodeStyleSettings.USE_SCALA3_INDENTATION_BASED_SYNTAX = true
  }

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.match.to.if")

  def testCallScrutineeAsMethodBodyIsWrappedInBlock(): Unit = doTest(
    s"""class Builder:
       |  def getTokenText: String = "x"
       |
       |object A:
       |  def back(builder: Builder): Unit =
       |    builder.getTokenText ${CARET}match
       |      case "a" => println("A")
       |      case "b" | "c" => println("BC")
       |      case _ => println("other")
       |""".stripMargin,
    s"""class Builder:
       |  def getTokenText: String = "x"
       |
       |object A:
       |  def back(builder: Builder): Unit =
       |    val text = builder.getTokenText
       |    if text == "a" then println("A")
       |    else if text == "b" || text == "c" then println("BC")
       |    else println("other")
       |""".stripMargin
  )

  def testCallScrutineeAsValBodyIsWrappedInBlock(): Unit = doTest(
    s"""object A:
       |  def g(): Int = 1
       |
       |  val r = g() ${CARET}match
       |    case 1 => "one"
       |    case _ => "other"
       |""".stripMargin,
    s"""object A:
       |  def g(): Int = 1
       |
       |  val r =
       |    val i = g()
       |    if i == 1 then "one"
       |    else "other"
       |""".stripMargin
  )
}
