package org.jetbrains.plugins.scala.codeInsight.hints

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.junit.Assert.assertEquals

abstract class InheritedDefaultArgumentHintsTestBase extends DefaultArgumentHintsTestBase {

  /** Only inherited default argument hints, without the call-site default argument hints. */
  protected def withInheritedDefaults[T](enabled: Boolean = true)(body: => T): T =
    inXRayMode(showDefaultArguments = false, showInheritedDefaultArguments = enabled)(body)

  override protected def doTest(text: String, showParameterNames: Boolean = false, withTooltips: Boolean = false): Unit =
    withInheritedDefaults() {
      doInlayTest(text, withTooltips)
    }
}

class InheritedDefaultArgumentHintsTest extends InheritedDefaultArgumentHintsTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  def testImplementedMethod(): Unit = doTest(
    s"""  trait Base {
       |    def foo(x: Int = 0, y: String = "a"): Unit
       |  }
       |
       |  class Impl extends Base {
       |    override def foo(x: Int$S = 0$E, y: String$S = "a"$E): Unit = ()
       |
       |    foo()
       |  }
       |""".stripMargin
  )

  def testOverriddenMethod(): Unit = doTest(
    s"""  class Base {
       |    def foo(x: Int, y: Int = 1): Unit = ()
       |  }
       |
       |  class Impl extends Base {
       |    override def foo(x: Int, y: Int$S = 1$E): Unit = ()
       |  }
       |""".stripMargin
  )

  def testOwnDefault(): Unit = doTest(
    s"""  class Base {
       |    def foo(x: Int = 0): Unit = ()
       |  }
       |
       |  class Impl extends Base {
       |    override def foo(x: Int = 1): Unit = ()
       |  }
       |""".stripMargin
  )

  def testMultipleParameterLists(): Unit = doTest(
    s"""  trait Base {
       |    def foo(x: Int)(y: Int = 2): Unit
       |  }
       |
       |  class Impl extends Base {
       |    def foo(x: Int)(y: Int$S = 2$E): Unit = ()
       |  }
       |""".stripMargin
  )

  def testTransitivelyInherited(): Unit = doTest(
    s"""  trait A {
       |    def foo(x: Int = 0): Unit
       |  }
       |
       |  trait B extends A {
       |    override def foo(x: Int$S = 0$E): Unit
       |  }
       |
       |  class C extends B {
       |    override def foo(x: Int$S = 0$E): Unit = ()
       |  }
       |""".stripMargin
  )

  def testComplexDefaultIsTruncated(): Unit = doTest(
    s"""  trait Base {
       |    def foo(x: String = "a very long default value that goes on", y: Int = {
       |      1 + 2
       |    }): Unit
       |  }
       |
       |  class Impl extends Base {
       |    def foo(x: String$S = "a very long defaul…$E, y: Int$S = { 1 + 2 }$E): Unit = ()
       |  }
       |""".stripMargin
  )

  def testNoHintsWithoutSuperMethod(): Unit = doTest(
    s"""  def foo(x: Int, y: Int = 1): Unit = ()
       |
       |  val f = (x: Int) => x
       |""".stripMargin
  )

  def testNoHintsWhenDisabled(): Unit = withInheritedDefaults(enabled = false) {
    doInlayTest(
      s"""  trait Base {
         |    def foo(x: Int = 0): Unit
         |  }
         |
         |  class Impl extends Base {
         |    override def foo(x: Int): Unit = ()
         |  }
         |""".stripMargin
    )
  }

  def testNavigation(): Unit = withInheritedDefaults() {
    val parts = inlayPartsIn(
      """  trait Base {
        |    def foo(x: Int = 42): Unit
        |  }
        |
        |  class Impl extends Base {
        |    override def foo(x: Int): Unit = ()
        |  }
        |""".stripMargin
    )

    def label(target: Any): String = target match {
      case element: PsiElement =>
        val owner = element.parentOfType(classOf[ScFunction]).flatMap(f => Option(f.containingClass)).map(_.name)
        s"${element.getText} in ${owner.getOrElse("?")}"
      case other => other.toString
    }

    val actual = parts.map(part => part.navigatable.fold(part.string)(target => s"${part.string}{${label(target)}}")).mkString
    assertEquals(" = 42{42 in Base}", actual)
  }
}

class InheritedDefaultArgumentHintsTest_Scala3 extends InheritedDefaultArgumentHintsTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3

  def testIssueExample(): Unit = doTest(
    s"""  abstract class Foo:
       |    def foo(x: Int = 0): Unit
       |
       |  class Bar extends Foo:
       |    def foo(x: Int$S = 0$E): Unit = {}
       |    foo()
       |""".stripMargin
  )
}
