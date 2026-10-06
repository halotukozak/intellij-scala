package org.jetbrains.plugins.scala
package codeInsight
package intention
package controlFlow

class IfToMatchIntentionTest extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.if.to.match")

  def testLiterals(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): String =
       |    i${CARET}f (n == 1) "one"
       |    else if (n == 2 || 3 == n) "two or three"
       |    else "many"
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): String =
       |    n match {
       |      case 1 => "one"
       |      case 2 | 3 => "two or three"
       |      case _ => "many"
       |    }
       |}""".stripMargin
  )

  def testBlocks(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): Unit = {
       |    var res = ""
       |    ${CARET}if (n == 1) {
       |      res = "one"
       |      println(res)
       |    } else if (n == 2) {
       |      res = "two"
       |    } else {
       |      // unknown
       |      res = "???"
       |    }
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): Unit = {
       |    var res = ""
       |    n match {
       |      case 1 =>
       |        res = "one"
       |        println(res)
       |      case 2 => res = "two"
       |      case _ =>
       |        // unknown
       |        res = "???"
       |    }
       |  }
       |}""".stripMargin
  )

  def testNoElse(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): Unit = {
       |    ${CARET}if (n == 1) println("one")
       |    else if (n == 2) println("two")
       |  }
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): Unit = {
       |    n match {
       |      case 1 => println("one")
       |      case 2 => println("two")
       |      case _ =>
       |    }
       |  }
       |}""".stripMargin
  )

  def testStableIdentifiers(): Unit = doTest(
    s"""object Colors {
       |  val Red = 1
       |  val green = 2
       |}
       |
       |class X {
       |  def f(n: Int, limit: Int): String = {
       |    val zero = 0
       |    ${CARET}if (n == Colors.Red) "red"
       |    else if (n == Colors.green) "green"
       |    else if (n == zero || n == limit) "zero or limit"
       |    else "other"
       |  }
       |}""".stripMargin,
    s"""object Colors {
       |  val Red = 1
       |  val green = 2
       |}
       |
       |class X {
       |  def f(n: Int, limit: Int): String = {
       |    val zero = 0
       |    n match {
       |      case Colors.Red => "red"
       |      case Colors.green => "green"
       |      case `zero` | `limit` => "zero or limit"
       |      case _ => "other"
       |    }
       |  }
       |}""".stripMargin
  )

  def testTypeTestsAndNull(): Unit = doTest(
    s"""class X {
       |  def f(a: Any): String =
       |    ${CARET}if (a eq null) "null"
       |    else if (a.isInstanceOf[String] || a.isInstanceOf[Int]) "string or int"
       |    else "other"
       |}""".stripMargin,
    s"""class X {
       |  def f(a: Any): String =
       |    a match {
       |      case null => "null"
       |      case _: String | _: Int => "string or int"
       |      case _ => "other"
       |    }
       |}""".stripMargin
  )

  def testGuard(): Unit = doTest(
    s"""class X {
       |  def f(n: Int, flag: Boolean): String =
       |    ${CARET}if (n == 1 && flag) "one"
       |    else if ((n == 2 || n == 3) && !flag && n > 0) "two or three"
       |    else "many"
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int, flag: Boolean): String =
       |    n match {
       |      case 1 if flag => "one"
       |      case 2 | 3 if !flag && n > 0 => "two or three"
       |      case _ => "many"
       |    }
       |}""".stripMargin
  )

  def testCaretOnElseIf(): Unit = doTest(
    s"""class X {
       |  def f(n: Int): String =
       |    if (n == 1) "one"
       |    else i${CARET}f (n == 2) "two"
       |    else "many"
       |}""".stripMargin,
    s"""class X {
       |  def f(n: Int): String =
       |    n match {
       |      case 1 => "one"
       |      case 2 => "two"
       |      case _ => "many"
       |    }
       |}""".stripMargin
  )

  // SCL-16885
  def testMethodCallSubject(): Unit = doTest(
    s"""class Builder {
       |  def getTokenText: String = ""
       |}
       |
       |class X {
       |  def f(builder: Builder): Unit = {
       |    ${CARET}if (builder.getTokenText == "1" || builder.getTokenText == "2") {
       |      println("one or two")
       |    } else if (builder.getTokenText == "3") {
       |      println("three")
       |    } else {
       |      println("other")
       |    }
       |  }
       |}""".stripMargin,
    s"""class Builder {
       |  def getTokenText: String = ""
       |}
       |
       |class X {
       |  def f(builder: Builder): Unit = {
       |    builder.getTokenText match {
       |      case "1" | "2" => println("one or two")
       |      case "3" => println("three")
       |      case _ => println("other")
       |    }
       |  }
       |}""".stripMargin
  )

  def testMethodCallWithArgumentsSubject(): Unit = doTest(
    s"""class X {
       |  def f(s: String): Int =
       |    ${CARET}if (s.charAt(0) == 'a') 1
       |    else if ((s.charAt( 0 )) == 'b' && s.length > 1) 2
       |    else if (s.charAt(0).isInstanceOf[Char]) 3
       |    else 4
       |}""".stripMargin,
    s"""class X {
       |  def f(s: String): Int =
       |    s.charAt(0) match {
       |      case 'a' => 1
       |      case 'b' if s.length > 1 => 2
       |      case _: Char => 3
       |      case _ => 4
       |    }
       |}""".stripMargin
  )

  def testNotAvailableMethodCallsWithDifferentArguments(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(s: String): Int =
       |    ${CARET}if (s.charAt(0) == 'a') 1
       |    else if (s.charAt(1) == 'b') 2
       |    else 3
       |}""".stripMargin
  )

  def testNotAvailableInstanceCreationSubject(): Unit = checkIntentionIsNotAvailable(
    s"""class A {
       |  val x: Int = 1
       |}
       |
       |class X {
       |  def f(): Int =
       |    ${CARET}if (new A().x == 1) 1
       |    else if (new A().x == 2) 2
       |    else 3
       |}""".stripMargin
  )

  def testNotAvailableDifferentSubjects(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(n: Int, m: Int): String =
       |    ${CARET}if (n == 1) "one"
       |    else if (m == 2) "two"
       |    else "many"
       |}""".stripMargin
  )

  def testNotAvailableNonEqualityCondition(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(n: Int): String =
       |    ${CARET}if (n == 1) "one"
       |    else if (n > 2) "many"
       |    else "two"
       |}""".stripMargin
  )

  def testNotAvailableUnstableIdentifier(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  var limit = 10
       |  def f(n: Int): String =
       |    ${CARET}if (n == limit) "limit"
       |    else if (n == 2) "two"
       |    else "other"
       |}""".stripMargin
  )

  def testNotAvailableEqWithNonNull(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(a: AnyRef, b: AnyRef): String =
       |    ${CARET}if (a eq b) "same"
       |    else "other"
       |}""".stripMargin
  )

  def testNotAvailableInBody(): Unit = checkIntentionIsNotAvailable(
    s"""class X {
       |  def f(n: Int): String =
       |    if (n == 1) "o${CARET}ne"
       |    else "many"
       |}""".stripMargin
  )
}

class IfToMatchIntentionTest_Scala3 extends intentions.ScalaIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_3

  override protected def setUp(): Unit = {
    super.setUp()
    getScalaCodeStyleSettings.USE_SCALA3_INDENTATION_BASED_SYNTAX = true
  }

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.if.to.match")

  def testIndentationSyntax(): Unit = doTest(
    s"""enum Color:
       |  case Red, Green, Blue
       |
       |def f(c: Color): String =
       |  ${CARET}if c == Color.Red then
       |    val s = "red"
       |    s
       |  else if c == Color.Green || c == Color.Blue then "green or blue"
       |  else "other"
       |""".stripMargin,
    s"""enum Color:
       |  case Red, Green, Blue
       |
       |def f(c: Color): String =
       |  c match
       |    case Color.Red =>
       |      val s = "red"
       |      s
       |    case Color.Green | Color.Blue => "green or blue"
       |    case _ => "other"
       |""".stripMargin
  )
}
