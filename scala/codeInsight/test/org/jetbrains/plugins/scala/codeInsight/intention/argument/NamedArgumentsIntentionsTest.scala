package org.jetbrains.plugins.scala
package codeInsight
package intention
package argument

import org.jetbrains.plugins.scala.extensions.executeWriteActionCommand

abstract class NamedArgumentsIntentionTestBase extends intentions.ScalaIntentionTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  // caret position after the intention is not checked
  override protected def checkIntentionResultText(expectedResultText: String)(originalInputForDebugging: String)
                                                 (implicit project: com.intellij.openapi.project.Project): Unit =
    super.checkIntentionResultText(expectedResultText.replace(CARET, ""))(originalInputForDebugging)
}

class AddNamesToCallArgumentsIntentionTest extends NamedArgumentsIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.add.names.to.call.arguments")

  def testOnCallee(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  f${CARET}oo(1, 2, 3)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  f${CARET}oo(a = 1, b = 2, c = 3)
       |}""".stripMargin
  )

  def testOnFirstArgument(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(${CARET}1, 2)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(${CARET}a = 1, b = 2)
       |}""".stripMargin
  )

  def testNotAvailableOnSecondArgument(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(1, ${CARET}2)
       |}""".stripMargin
  )

  def testSomeArgumentsAlreadyNamed(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  ${CARET}foo(1, b = 2, 3)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  ${CARET}foo(a = 1, b = 2, c = 3)
       |}""".stripMargin
  )

  def testNotAvailableWhenAllNamed(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  ${CARET}foo(b = 2, a = 1)
       |}""".stripMargin
  )

  def testStopAtVarargs(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(1, 2, 3)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(a = 1, 2, 3)
       |}""".stripMargin
  )

  def testStopAtVarargsSplice(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(1, Seq(2, 3): _*)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(a = 1, Seq(2, 3): _*)
       |}""".stripMargin
  )

  def testNotAvailableForVarargsOnly(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(xs: Int*): Unit = ()
       |
       |  ${CARET}foo(1, 2)
       |}""".stripMargin
  )

  def testSecondArgumentList(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)(${CARET}2, 3)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)(${CARET}b = 2, c = 3)
       |}""".stripMargin
  )

  def testConstructor(): Unit = doTest(
    s"""class A(a: Int, b: String)
       |
       |object Test {
       |  new ${CARET}A(1, "2")
       |}""".stripMargin,
    s"""class A(a: Int, b: String)
       |
       |object Test {
       |  new ${CARET}A(a = 1, b = "2")
       |}""".stripMargin
  )

  def testCaseClassApply(): Unit = doTest(
    s"""case class A(a: Int, b: String)
       |
       |object Test {
       |  ${CARET}A(1, "2")
       |}""".stripMargin,
    s"""case class A(a: Int, b: String)
       |
       |object Test {
       |  ${CARET}A(a = 1, b = "2")
       |}""".stripMargin
  )

  def testNotAvailableForJava(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  val list: java.util.List[String] = ???
       |  list.${CARET}add(0, "123")
       |}""".stripMargin
  )
}

class RemoveAllArgumentNamesIntentionTest extends NamedArgumentsIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.remove.all.argument.names")

  def testReorder(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  ${CARET}foo(c = 3, a = 1, b = 2)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  ${CARET}foo(1, 2, 3)
       |}""".stripMargin,
    Some(ScalaCodeInsightBundle.message("family.name.remove.all.argument.names"))
  )

  def testInsideArguments(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(b = 2, a = ${CARET}1)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(${CARET}1, 2)
       |}""".stripMargin
  )

  def testKeepNamesAfterSkippedDefault(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int = 0, d: Int): Unit = ()
       |
       |  ${CARET}foo(d = 4, b = 2, a = 1)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int = 0, d: Int): Unit = ()
       |
       |  ${CARET}foo(1, 2, d = 4)
       |}""".stripMargin,
    Some(ScalaCodeInsightBundle.message("remove.all.possible.argument.names"))
  )

  def testNotAvailableWhenDefaultIsSkipped(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  ${CARET}foo(c = 3, a = 1)
       |}""".stripMargin
  )

  def testNotAvailableForSingleName(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  ${CARET}foo(1, b = 2)
       |}""".stripMargin
  )

  def testTrailingDefault(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int = 0): Unit = ()
       |
       |  ${CARET}foo(b = 2, a = 1)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int = 0): Unit = ()
       |
       |  ${CARET}foo(1, 2)
       |}""".stripMargin
  )

  def testVarargs(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(b = 2, a = 1)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(1, 2)
       |}""".stripMargin
  )

  def testConstructor(): Unit = doTest(
    s"""class A(a: Int, b: String)
       |
       |object Test {
       |  new ${CARET}A(b = "2", a = 1)
       |}""".stripMargin,
    s"""class A(a: Int, b: String)
       |
       |object Test {
       |  new ${CARET}A(1, "2")
       |}""".stripMargin
  )
}

class RemoveSingleArgumentNameIntentionTest extends NamedArgumentsIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.remove.argument.name")

  def testInPlace(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(a = 1, ${CARET}b = 2)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(a = 1, ${CARET}2)
       |}""".stripMargin,
    Some(ScalaCodeInsightBundle.message("remove.argument.name.0", "b"))
  )

  def testMoveToPosition(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  foo(a = 1, c = 3, ${CARET}b = 2)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  foo(a = 1, 2, ${CARET}c = 3)
       |}""".stripMargin
  )

  def testMoveToFront(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(b = 2, a$CARET = 1)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(1, b = 2$CARET)
       |}""".stripMargin
  )

  def testNotAvailableWhenDefaultIsSkipped(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  foo(a = 1, ${CARET}c = 3)
       |}""".stripMargin
  )

  def testNotAvailableInValue(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(a = 1, b = ${CARET}2)
       |}""".stripMargin
  )

  def testNotAvailableWhenPrecedingIsNotInPlace(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int, c: Int): Unit = ()
       |
       |  foo(c = 3, ${CARET}b = 2, a = 1)
       |}""".stripMargin
  )
}

class SpecifyRemainingRequiredArgumentsByNameIntentionTest extends NamedArgumentsIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.specify.remaining.required.arguments.by.name")

  def testEmptyArguments(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  ${CARET}foo()
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  foo(a = ???, c = ???)
       |}""".stripMargin
  )

  def testAfterPositionalArgument(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  foo(1$CARET)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  foo(1, c = ???)
       |}""".stripMargin
  )

  def testCaseClassApply(): Unit = doTest(
    s"""case class A(a: Int, b: Int = 0)
       |
       |object Test {
       |  ${CARET}A()
       |}""".stripMargin,
    s"""case class A(a: Int, b: Int = 0)
       |
       |object Test {
       |  A(a = ???)
       |}""".stripMargin
  )

  def testNotAvailableWhenSameAsAll(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  ${CARET}foo()
       |}""".stripMargin
  )

  def testNotAvailableInsideArgument(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |  def bar: Int = 1
       |
       |  foo(b${CARET}ar)
       |}""".stripMargin
  )
}

class SpecifyAllRemainingArgumentsByNameIntentionTest extends NamedArgumentsIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.specify.all.remaining.arguments.by.name")

  def testEmptyArguments(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  ${CARET}foo()
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  foo(a = ???, b = ???, c = ???)
       |}""".stripMargin
  )

  def testInsideParentheses(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo($CARET)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int): Unit = ()
       |
       |  foo(a = ???, b = ???)
       |}""".stripMargin
  )

  def testAfterNamedArgument(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  ${CARET}foo(c = 3)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, b: Int = 0, c: Int): Unit = ()
       |
       |  foo(c = 3, a = ???, b = ???)
       |}""".stripMargin
  )

  def testSecondArgumentList(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)($CARET)
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)(b = ???, c = ???)
       |}""".stripMargin
  )

  def testConstructor(): Unit = doTest(
    s"""class A(a: Int, b: String)
       |
       |object Test {
       |  new ${CARET}A()
       |}""".stripMargin,
    s"""class A(a: Int, b: String)
       |
       |object Test {
       |  new A(a = ???, b = ???)
       |}""".stripMargin
  )

  def testSkipVarargs(): Unit = doTest(
    s"""object Test {
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo()
       |}""".stripMargin,
    s"""object Test {
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  foo(a = ???)
       |}""".stripMargin
  )

  def testNotAvailableWhenNoRequiredMissing(): Unit = checkIntentionIsNotAvailable(
    s"""object Test {
       |  def foo(a: Int, b: Int = 0): Unit = ()
       |
       |  ${CARET}foo(1)
       |}""".stripMargin
  )
}

class NamedArgumentsIntentionsTest_Scala3 extends NamedArgumentsIntentionTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3

  // the formatter inserts a space before `*` of a vararg splice
  override protected def formatIntentionResultBeforeChecking: Boolean = false

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.add.names.to.call.arguments")

  private def specifyAllFamilyName = ScalaCodeInsightBundle.message("family.name.specify.all.remaining.arguments.by.name")

  private def removeAllFamilyName = ScalaCodeInsightBundle.message("family.name.remove.all.argument.names")

  private def checkResult(familyName: String, text: String, result: String): Unit = {
    myFixture.configureByText(fileType, text)
    val action = findIntentionByName(familyName).getOrElse(throw new AssertionError(s"Intention `$familyName` not found"))
    executeWriteActionCommand("Test Intention") {
      action.invoke(getProject, getEditor, getFile)
    }(getProject)
    checkIntentionResultText(result)(text)(getProject)
  }

  def testVarargsSplice(): Unit = doTest(
    s"""object Test:
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(1, Seq(2, 3)*)
       |""".stripMargin,
    s"""object Test:
       |  def foo(a: Int, xs: Int*): Unit = ()
       |
       |  ${CARET}foo(a = 1, Seq(2, 3)*)
       |""".stripMargin
  )

  def testNotAvailableInUsingArguments(): Unit = checkIntentionIsNotAvailable(
    s"""object Test:
       |  def foo(a: Int)(using x: String, y: Int): Unit = ()
       |
       |  foo(1)(using "x$CARET", 2)
       |""".stripMargin
  )

  def testUniversalApply(): Unit = checkResult(removeAllFamilyName,
    s"""class A(a: Int, b: String)
       |
       |object Test:
       |  ${CARET}A(b = "2", a = 1)
       |""".stripMargin,
    s"""class A(a: Int, b: String)
       |
       |object Test:
       |  A(1, "2")
       |""".stripMargin
  )

  def testSpecifyAfterUsingClause(): Unit = checkResult(specifyAllFamilyName,
    s"""object Test:
       |  given String = ""
       |
       |  def foo(a: Int)(using x: String)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)($CARET)
       |""".stripMargin,
    s"""object Test:
       |  given String = ""
       |
       |  def foo(a: Int)(using x: String)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)(b = ???, c = ???)
       |""".stripMargin
  )

  def testSpecifyAfterExplicitUsingClause(): Unit = checkResult(specifyAllFamilyName,
    s"""object Test:
       |  def foo(a: Int)(using x: String)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)(using "")($CARET)
       |""".stripMargin,
    s"""object Test:
       |  def foo(a: Int)(using x: String)(b: Int, c: Int): Unit = ()
       |
       |  foo(1)(using "")(b = ???, c = ???)
       |""".stripMargin
  )
}
