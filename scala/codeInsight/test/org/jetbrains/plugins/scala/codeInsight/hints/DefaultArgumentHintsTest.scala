package org.jetbrains.plugins.scala.codeInsight.hints

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInsight.{InlayHintsTestBase, ScalaCodeInsightSettings}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.settings.ScalaApplicationSettings
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaVersion}
import org.junit.Assert.assertEquals

abstract class DefaultArgumentHintsTestBase extends InlayHintsTestBase {

  protected final def S: String = Hint.Start

  protected final def E: String = Hint.End

  /**
   * Runs `body` in X-Ray mode with only the hints under test (and, optionally, parameter name hints) enabled,
   * so that the expected text is not cluttered by other X-Ray hints.
   */
  protected def inXRayMode[T](
    showDefaultArguments:          Boolean = true,
    showInheritedDefaultArguments: Boolean = false,
    showParameterNames:            Boolean = false
  )(body: => T): T = {
    val settings = ScalaApplicationSettings.getInstance
    val codeInsightSettings = ScalaCodeInsightSettings.getInstance

    val oldXRayMode = ScalaHintsSettings.xRayMode
    val oldDefaultArguments = settings.XRAY_SHOW_DEFAULT_ARGUMENT_HINTS
    val oldInheritedDefaultArguments = settings.XRAY_SHOW_INHERITED_DEFAULT_ARGUMENT_HINTS
    val oldParameterHints = settings.XRAY_SHOW_PARAMETER_HINTS
    val oldTypeHints = settings.XRAY_SHOW_TYPE_HINTS
    val oldApplyMethodHints = settings.XRAY_SHOW_APPLY_METHOD_HINTS
    val oldParameterNames = codeInsightSettings.showParameterNames
    try {
      ScalaHintsSettings.xRayMode = true
      settings.XRAY_SHOW_DEFAULT_ARGUMENT_HINTS = showDefaultArguments
      settings.XRAY_SHOW_INHERITED_DEFAULT_ARGUMENT_HINTS = showInheritedDefaultArguments
      settings.XRAY_SHOW_PARAMETER_HINTS = showParameterNames
      settings.XRAY_SHOW_TYPE_HINTS = false
      settings.XRAY_SHOW_APPLY_METHOD_HINTS = false
      codeInsightSettings.showParameterNames = false
      body
    } finally {
      ScalaHintsSettings.xRayMode = oldXRayMode
      settings.XRAY_SHOW_DEFAULT_ARGUMENT_HINTS = oldDefaultArguments
      settings.XRAY_SHOW_INHERITED_DEFAULT_ARGUMENT_HINTS = oldInheritedDefaultArguments
      settings.XRAY_SHOW_PARAMETER_HINTS = oldParameterHints
      settings.XRAY_SHOW_TYPE_HINTS = oldTypeHints
      settings.XRAY_SHOW_APPLY_METHOD_HINTS = oldApplyMethodHints
      codeInsightSettings.showParameterNames = oldParameterNames
    }
  }

  protected def doTest(text: String, showParameterNames: Boolean = false, withTooltips: Boolean = false): Unit =
    inXRayMode(showParameterNames = showParameterNames) {
      doInlayTest(text, withTooltips)
    }
}

class DefaultArgumentHintsTest extends DefaultArgumentHintsTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  def testOmittedTrailingArguments(): Unit = doTest(
    s"""  def connect(host: String, port: Int = 8080, secure: Boolean = false): Unit = ()
       |
       |  connect("a"$S, 8080, false$E)
       |""".stripMargin
  )

  def testAllArgumentsPassed(): Unit = doTest(
    s"""  def connect(host: String, port: Int = 8080, secure: Boolean = false): Unit = ()
       |
       |  connect("a", 1, true)
       |""".stripMargin
  )

  def testEmptyArgumentList(): Unit = doTest(
    s"""  def connect(port: Int = 8080, secure: Boolean = false): Unit = ()
       |
       |  connect(${S}8080, false$E)
       |""".stripMargin
  )

  def testNamedArguments(): Unit = doTest(
    s"""  def connect(host: String, port: Int = 8080, secure: Boolean = false): Unit = ()
       |
       |  connect("a", secure = true$S, port = 8080$E)
       |  connect(host = "a", port = 1$S, secure = false$E)
       |""".stripMargin
  )

  def testParameterNamesWhenParameterNameHintsAreShown(): Unit = doTest(
    s"""  def connect(host: String, port: Int = 8080, secure: Boolean = false): Unit = ()
       |
       |  connect(${S}host = $E"a"$S, port = 8080, secure = false$E)
       |""".stripMargin,
    showParameterNames = true
  )

  def testMultipleParameterLists(): Unit = doTest(
    s"""  def f(a: Int = 1)(b: Int, c: String = "c"): Unit = ()
       |
       |  f(${S}1$E)(2$S, "c"$E)
       |  f(0)(b = 2$S, c = "c"$E)
       |""".stripMargin
  )

  def testConstructor(): Unit = doTest(
    s"""  class Server(host: String, port: Int = 8080)
       |
       |  new Server("a"$S, 8080$E)
       |""".stripMargin
  )

  def testConstructorWithoutArgumentList(): Unit = doTest(
    s"""  class Point(x: Int = 0, y: Int = 0)
       |
       |  class Point3D extends Point$S(0, 0)$E
       |
       |  new Point$S(0, 0)$E
       |""".stripMargin
  )

  def testCaseClassApply(): Unit = doTest(
    s"""  case class Point(x: Int = 0, y: Int = 0)
       |
       |  Point(1$S, 0$E)
       |  new Point(${S}0, 0$E)
       |""".stripMargin
  )

  def testNoHintsForCaseClassCopy(): Unit = doTest(
    s"""  case class Point(x: Int = 0, y: Int = 0)
       |
       |  Point(1, 2).copy(x = 3)
       |""".stripMargin
  )

  def testLongDefaultIsTruncated(): Unit = doTest(
    s"""  def f(a: String = "a very long default value that goes on"): Unit = ()
       |
       |  f($S"a very long defaul…$E)
       |""".stripMargin
  )

  def testNegativeLiteral(): Unit = doTest(
    s"""  def f(a: Int = -1): Unit = ()
       |
       |  f(${S}-1$E)
       |""".stripMargin
  )

  def testComplexDefaultIsSynthetic(): Unit = doTest(
    s"""  def f(a: Int, b: Int = a + 1): Unit = ()
       |
       |  f(1$S, default$$2$E)
       |""".stripMargin
  )

  def testComplexDefaultTooltip(): Unit = doTest(
    s"""  def f(a: Int, b: Int = a + 1): Unit = ()
       |
       |  f(1$S, default$$2 /* a + 1 */ $E)
       |""".stripMargin,
    withTooltips = true
  )

  def testReferenceToParameterIsSynthetic(): Unit = doTest(
    s"""  def f(a: Int, b: Int = a): Unit = ()
       |
       |  f(1$S, default$$2$E)
       |""".stripMargin
  )

  def testSyntheticIndexSpansParameterLists(): Unit = doTest(
    s"""  def f(a: Int = 1)(b: Int = math.max(1, 2)): Unit = ()
       |
       |  f(${S}1$E)(${S}default$$2$E)
       |""".stripMargin
  )

  def testAccessibleAndInaccessibleReferences(): Unit = doTest(
    s"""  object Defaults {
       |    val Port = 8080
       |  }
       |
       |  class Server {
       |    private val secret = 42
       |
       |    def connect(port: Int = Defaults.Port, key: Int = secret): Unit = ()
       |
       |    connect(${S}Defaults.Port, secret$E)
       |  }
       |
       |  new Server().connect(${S}Defaults.Port, default$$2$E)
       |""".stripMargin
  )

  def testShadowedReferenceIsSynthetic(): Unit = doTest(
    s"""  val port = 8080
       |
       |  def connect(p: Int = port): Unit = ()
       |
       |  def test(port: String): Unit = {
       |    connect(${S}default$$1$E)
       |  }
       |
       |  connect(${S}port$E)
       |""".stripMargin
  )

  def testDefaultFromOverriddenMethod(): Unit = doTest(
    s"""  trait Base { def f(a: Int = 42): Unit }
       |
       |  class Impl extends Base { override def f(a: Int): Unit = () }
       |
       |  new Impl().f(${S}42$E)
       |""".stripMargin
  )

  def testNoHintsForImplicitClause(): Unit = doTest(
    s"""  def f(a: Int)(implicit b: Int = 1): Unit = ()
       |
       |  f(1)
       |""".stripMargin
  )

  def testNoHintsWhenDisabled(): Unit = inXRayMode(showDefaultArguments = false) {
    doInlayTest(
      s"""  def connect(host: String, port: Int = 8080): Unit = ()
         |
         |  connect("a")
         |""".stripMargin
    )
  }

  def testNoHintsOutsideXRayMode(): Unit = inXRayMode() {
    ScalaHintsSettings.xRayMode = false
    doInlayTest(
      s"""  def connect(host: String, port: Int = 8080): Unit = ()
         |
         |  connect("a")
         |""".stripMargin
    )
  }

  def testNavigation(): Unit = inXRayMode(showParameterNames = true) {
    val parts = inlayPartsIn(
      """  def connect(port: Int = 8080, key: Int = port + 1): Unit = ()
        |
        |  connect()
        |""".stripMargin
    )

    def label(target: Any): String = target match {
      case parameter: ScParameter => s"param ${parameter.name}"
      case element: PsiElement => s"expression ${element.getText}"
      case other => other.toString
    }

    val actual = parts.map(part => part.navigatable.fold(part.string)(target => s"${part.string}{${label(target)}}")).mkString
    assertEquals("port{param port} = 8080{expression 8080}, key{param key} = default$2{expression port + 1}", actual)
  }
}

class DefaultArgumentHintsTest_Scala3 extends DefaultArgumentHintsTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3

  def testUniversalApply(): Unit = doTest(
    s"""  class Server(host: String, port: Int = 8080)
       |
       |  Server("a"$S, 8080$E)
       |""".stripMargin
  )

  def testUsingClause(): Unit = doTest(
    s"""  def f(a: Int)(using b: Int = 1, c: Int = 2): Unit = ()
       |
       |  f(1)(using 5$S, 2$E)
       |""".stripMargin
  )

  def testEnumCase(): Unit = doTest(
    s"""  enum Shape:
       |    case Circle(radius: Double = 1.0)
       |
       |  Shape.Circle(${S}1.0$E)
       |""".stripMargin
  )
}
