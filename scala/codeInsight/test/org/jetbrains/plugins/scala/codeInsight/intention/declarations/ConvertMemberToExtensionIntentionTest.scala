package org.jetbrains.plugins.scala.codeInsight.intention.declarations

import com.intellij.refactoring.util.CommonRefactoringUtil.RefactoringErrorHintException
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.codeInsight.{ScalaCodeInsightBundle, intentions}
import org.jetbrains.plugins.scala.extensions.executeWriteActionCommand
import org.junit.Assert.fail

class ConvertMemberToExtensionIntentionTest extends intentions.ScalaIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.member.to.extension")

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= ScalaVersion.Latest.Scala_3

  def testSimple(): Unit = doTest(
    s"""class Foo {
       |  val x: Int = 1
       |
       |  def ba${CARET}r(y: Int): Int = x + y
       |}
       |""".stripMargin,
    s"""class Foo {
       |  val x: Int = 1
       |}
       |
       |object Foo {
       |  extension (foo: Foo) def bar(y: Int): Int = foo.x + y
       |}
       |""".stripMargin
  )

  def testGenericClass(): Unit = doTest(
    s"""class Box[+T](val value: T) {
       |  def get${CARET}Value: T = value
       |}
       |""".stripMargin,
    s"""class Box[+T](val value: T) {
       |}
       |
       |object Box {
       |  extension [T](box: Box[T]) def getValue: T = box.value
       |}
       |""".stripMargin
  )

  def testGenericMethodWithBounds(): Unit = doTest(
    s"""class Box[T <: AnyRef](val value: T) {
       |  def ma${CARET}p[U](f: T => U): U = f(value)
       |}
       |""".stripMargin,
    s"""class Box[T <: AnyRef](val value: T) {
       |}
       |
       |object Box {
       |  extension [T <: AnyRef](box: Box[T]) def map[U](f: T => U): U = f(box.value)
       |}
       |""".stripMargin
  )

  def testThisReference(): Unit = doTest(
    s"""class Foo {
       |  def is${CARET}Same(other: Foo): Boolean = this == other
       |}
       |""".stripMargin,
    s"""class Foo {
       |}
       |
       |object Foo {
       |  extension (foo: Foo) def isSame(other: Foo): Boolean = foo == other
       |}
       |""".stripMargin
  )

  def testInterpolatedString(): Unit = doTest(
    s"""class Foo(val name: String) {
       |  def gre${CARET}et: String = s"Hello, $$name"
       |}
       |""".stripMargin,
    s"""class Foo(val name: String) {
       |}
       |
       |object Foo {
       |  extension (foo: Foo) def greet: String = s"Hello, $${foo.name}"
       |}
       |""".stripMargin
  )

  def testReceiverNameClash(): Unit = doTest(
    s"""class Foo {
       |  val x = 1
       |  def ba${CARET}r(foo: Int): Int = foo + x
       |}
       |""".stripMargin,
    s"""class Foo {
       |  val x = 1
       |}
       |
       |object Foo {
       |  extension (foo1: Foo) def bar(foo: Int): Int = foo + foo1.x
       |}
       |""".stripMargin
  )

  def testRecursionAndUsagesInsideClass(): Unit = doTest(
    s"""class Counter {
       |  def run(): Int = twice(1)
       |
       |  def tw${CARET}ice(n: Int): Int = if (n > 10) n else twice(n * 2)
       |}
       |""".stripMargin,
    s"""class Counter {
       |  def run(): Int = this.twice(1)
       |}
       |
       |object Counter {
       |  extension (counter: Counter) def twice(n: Int): Int = if (n > 10) n else counter.twice(n * 2)
       |}
       |""".stripMargin
  )

  def testUsageInSubclass(): Unit = doTest(
    s"""trait Base {
       |  def n: Int
       |  def dou${CARET}ble: Int = n * 2
       |}
       |
       |class Impl extends Base {
       |  def n: Int = 21
       |  def answer: Int = double
       |}
       |""".stripMargin,
    s"""trait Base {
       |  def n: Int
       |}
       |
       |object Base {
       |  extension (base: Base) def double: Int = base.n * 2
       |}
       |
       |class Impl extends Base {
       |  def n: Int = 21
       |
       |  def answer: Int = this.double
       |}
       |""".stripMargin
  )

  def testPrivateMemberIsAccessibleFromCompanion(): Unit = doTest(
    s"""class Foo {
       |  private val secret = 42
       |  def rev${CARET}eal: Int = secret
       |}
       |""".stripMargin,
    s"""class Foo {
       |  private val secret = 42
       |}
       |
       |object Foo {
       |  extension (foo: Foo) def reveal: Int = foo.secret
       |}
       |""".stripMargin
  )

  def testExistingCompanion(): Unit = doTest(
    s"""class Foo(val x: Int) {
       |  def plu${CARET}s(y: Int): Int = x + y
       |}
       |
       |object Foo {
       |  val zero: Foo = new Foo(0)
       |}
       |""".stripMargin,
    s"""class Foo(val x: Int) {
       |}
       |
       |object Foo {
       |  val zero: Foo = new Foo(0)
       |  extension (foo: Foo) def plus(y: Int): Int = foo.x + y
       |}
       |""".stripMargin
  )

  def testBracelessClass(): Unit = doTest(
    s"""class Foo(val x: Int):
       |  def twi${CARET}ce: Int =
       |    x * 2
       |""".stripMargin,
    s"""class Foo(val x: Int)
       |
       |object Foo {
       |  extension (foo: Foo) def twice: Int =
       |    foo.x * 2
       |}
       |""".stripMargin
  )

  def testScalaDocIsKept(): Unit = doTest(
    s"""class Foo {
       |  /** Doubles the value */
       |  def twi${CARET}ce(x: Int): Int = x * 2
       |}
       |""".stripMargin,
    s"""class Foo {
       |}
       |
       |object Foo {
       |  /** Doubles the value */
       |  extension (foo: Foo) def twice(x: Int): Int = x * 2
       |}
       |""".stripMargin
  )

  def testNotAvailableForAbstractMethod(): Unit = checkIntentionIsNotAvailable(
    s"""trait Foo {
       |  def ba${CARET}r: Int
       |}
       |""".stripMargin
  )

  def testNotAvailableForOverride(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  override def toStr${CARET}ing: String = "foo"
       |}
       |""".stripMargin
  )

  def testNotAvailableForImplementation(): Unit = checkIntentionIsNotAvailable(
    s"""trait Base {
       |  def bar: Int
       |}
       |class Foo extends Base {
       |  def ba${CARET}r: Int = 1
       |}
       |""".stripMargin
  )

  def testNotAvailableForPrivateMethod(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  private def ba${CARET}r: Int = 1
       |}
       |""".stripMargin
  )

  def testNotAvailableInObject(): Unit = checkIntentionIsNotAvailable(
    s"""object Foo {
       |  def ba${CARET}r: Int = 1
       |}
       |""".stripMargin
  )

  def testNotAvailableForLocalMethod(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  def foo: Int = {
       |    def ba${CARET}r: Int = 1
       |    bar
       |  }
       |}
       |""".stripMargin
  )

  def testNotAvailableOutsideOfName(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  def bar: Int = 1$CARET
       |}
       |""".stripMargin
  )

  def testNotAvailableWithSuperCall(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  def ha${CARET}sh: Int = super.hashCode()
       |}
       |""".stripMargin
  )

  def testNotAvailableWithNonMemberClassParameter(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo(x: Int) {
       |  def ba${CARET}r: Int = x
       |}
       |""".stripMargin
  )

  def testNotAvailableWithPrivateThisMember(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  private[this] val x = 1
       |  def ba${CARET}r: Int = x
       |}
       |""".stripMargin
  )

  def testNotAvailableWithTypeMember(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  type T = Int
       |  def ba${CARET}r: T = 1
       |}
       |""".stripMargin
  )

  def testOverriddenMethodShowsError(): Unit = {
    myFixture.configureByText("a.scala",
      s"""class Foo {
         |  def ba${CARET}r: Int = 1
         |}
         |class Bar extends Foo {
         |  override def bar: Int = 2
         |}
         |""".stripMargin
    )
    val intention = findIntentionByName(familyName).getOrElse(fail("Intention is not available").asInstanceOf[Nothing])
    try {
      executeWriteActionCommand("Test Intention") {
        intention.invoke(getProject, getEditor, getFile)
      }(getProject)
      fail("Expected error hint")
    } catch {
      case _: RefactoringErrorHintException =>
    }
  }
}

class ConvertMemberToExtensionIntentionTest_Scala2 extends intentions.ScalaIntentionTestBase {

  override def familyName: String = ScalaCodeInsightBundle.message("family.name.convert.member.to.extension")

  override protected def supportedIn(version: ScalaVersion): Boolean = version == ScalaVersion.Latest.Scala_2_13

  def testNotAvailableInScala2(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo {
       |  def ba${CARET}r: Int = 1
       |}
       |""".stripMargin
  )
}
