package org.jetbrains.plugins.scala.codeInsight.hints.lambdas

import com.intellij.codeInsight.hints.InlayDumpUtil
import com.intellij.codeInsight.hints.declarative.InlayProviderPassInfo
import com.intellij.codeInsight.hints.declarative.impl.DeclarativeInlayHintsPass
import com.intellij.codeInsight.hints.declarative.impl.util.DeclarativeHintsDumpUtil
import com.intellij.codeInsight.hints.declarative.impl.views.{InlayPresentationList, TextInlayPresentationEntry}
import com.intellij.codeInsight.multiverse.CodeInsightContextUtil
import com.intellij.openapi.actionSystem.ex.ActionUtil
import com.intellij.openapi.progress.EmptyProgressIndicator
import com.intellij.openapi.util.Computable
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestCase
import org.jetbrains.plugins.scala.{EditorTests, LatestScalaVersions, ScalaVersion}
import org.junit.Assert.assertEquals
import org.junit.experimental.categories.Category

import java.util.Collections

@Category(Array(classOf[EditorTests]))
abstract class ScalaLambdaReturnHintsTestBase extends ScalaLightCodeInsightFixtureTestCase {

  /** Wraps a test snippet into a compilation unit valid for the tested Scala version. */
  protected def wrap(code: String): String = code

  protected def doTest(text: String): Unit = {
    val expected = wrap(text)
    val source = InlayDumpUtil.INSTANCE.removeInlays(expected)
    configureFromFileText(source)

    val file = getFile
    val editor = getEditor
    val info = new InlayProviderPassInfo(
      new ScalaLambdaReturnHintsProvider,
      ScalaLambdaReturnHintsProvider.ProviderId,
      Collections.emptyMap()
    )
    val pass = ActionUtil.underModalProgress(getProject, "", (() =>
      new DeclarativeInlayHintsPass(file, editor, Collections.singletonList(info), false, false)
    ): Computable[DeclarativeInlayHintsPass])
    pass.setContext(CodeInsightContextUtil.getCodeInsightContext(file))
    ActionUtil.underModalProgress(getProject, "", (() =>
      pass.doCollectInformation(new EmptyProgressIndicator)
    ): Computable[Unit])
    pass.applyInformationToEditor()

    val dump = DeclarativeHintsDumpUtil.dumpHints(source, editor, (list: InlayPresentationList) =>
      list.getEntries.collect { case entry: TextInlayPresentationEntry => entry.getText }.mkString
    )
    assertEquals(expected.trim, dump.trim)
  }
}

class ScalaLambdaReturnHintsTest extends ScalaLambdaReturnHintsTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  override protected def wrap(code: String): String =
    s"""object Test {
       |$code
       |}""".stripMargin

  def testFunctionLiteralInBraces(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  val y = x * 2
      |  y + 1/*<# ^map #>*/
      |}
      |""".stripMargin
  )

  def testFunctionLiteralWithBlockBody(): Unit = doTest(
    """Seq(1, 2).map(x => {
      |  val y = x * 2
      |  y + 1/*<# ^map #>*/
      |})
      |""".stripMargin
  )

  def testSingleStatementLambda(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  x + 1
      |}
      |Seq(1, 2).map(x => x + 1)
      |""".stripMargin
  )

  def testCurriedCall(): Unit = doTest(
    """Seq(1, 2).foldLeft(0) { (acc, x) =>
      |  val y = x * 2
      |  acc + y/*<# ^foldLeft #>*/
      |}
      |""".stripMargin
  )

  def testInfixCall(): Unit = doTest(
    """Seq(1, 2) map { x =>
      |  val y = x * 2
      |  y/*<# ^map #>*/
      |}
      |""".stripMargin
  )

  def testUnitLambda(): Unit = doTest(
    """Seq(1, 2).foreach { x =>
      |  val y = x * 2
      |  println(y)
      |}
      |""".stripMargin
  )

  def testMultiLineIf(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  val y = x * 2
      |  if (y > 2)
      |    "big"/*<# ^map #>*/
      |  else
      |    "small"/*<# ^map #>*/
      |}
      |""".stripMargin
  )

  def testMultiLineIfAsTheOnlyStatement(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  if (x > 1)
      |    "big"/*<# ^map #>*/
      |  else
      |    "small"/*<# ^map #>*/
      |}
      |""".stripMargin
  )

  def testMultiLineMatchAsTheOnlyStatement(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  x match {
      |    case 1 => "one"/*<# ^map #>*/
      |    case _ => "other"/*<# ^map #>*/
      |  }
      |}
      |""".stripMargin
  )

  def testOneLineIfAsTheOnlyStatement(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  if (x > 1) "big" else "small"
      |}
      |""".stripMargin
  )

  def testOneLineIf(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  val y = x * 2
      |  if (y > 2) "big" else "small"/*<# ^map #>*/
      |}
      |""".stripMargin
  )

  def testMatch(): Unit = doTest(
    """Seq(1, 2).map { x =>
      |  val y = x * 2
      |  y match {
      |    case 2 => "two"/*<# ^map #>*/
      |    case _ =>
      |      val s = y.toString
      |      s/*<# ^map #>*/
      |  }
      |}
      |""".stripMargin
  )

  def testCaseClauses(): Unit = doTest(
    """Seq(1 -> 2).map {
      |  case (a, b) =>
      |    val sum = a + b
      |    sum * 2/*<# ^map #>*/
      |}
      |Seq(1 -> 2).map { case (a, b) => a + b }
      |""".stripMargin
  )

  def testByNameBlock(): Unit = doTest(
    """def lazily[A](a: => A): A = a
      |
      |lazily {
      |  val x = 1
      |  x + 1/*<# ^lazily #>*/
      |}
      |""".stripMargin
  )

  def testNoHintsForByValueBlock(): Unit = doTest(
    """def strictly[A](a: A): A = a
      |
      |strictly {
      |  val x = 1
      |  x + 1
      |}
      |""".stripMargin
  )

  def testLambdaNotPassedToMethod(): Unit = doTest(
    """val f = (x: Int) => {
      |  val y = x * 2
      |  y + 1/*<# ^lambda #>*/
      |}
      |""".stripMargin
  )

  def testNestedLambdas(): Unit = doTest(
    """Seq(Seq(1)).map { xs =>
      |  val ys = xs.filter { x =>
      |    val y = x * 2
      |    y > 1/*<# ^filter #>*/
      |  }
      |  ys.size/*<# ^map #>*/
      |}
      |""".stripMargin
  )
}

class ScalaLambdaReturnHintsTest_Scala3 extends ScalaLambdaReturnHintsTestBase {

  override protected def supportedIn(version: ScalaVersion): Boolean = version.isScala3

  def testColonArgument(): Unit = doTest(
    """val r = Seq(1, 2).map: x =>
      |  val y = x * 2
      |  y + 1/*<# ^map #>*/
      |""".stripMargin
  )

  def testIndentedIfAsTheOnlyStatement(): Unit = doTest(
    """val r = Seq(1, 2).map { x =>
      |  if x > 1 then
      |    "big"/*<# ^map #>*/
      |  else
      |    "small"/*<# ^map #>*/
      |}
      |""".stripMargin
  )

  def testIndentedBody(): Unit = doTest(
    """val r = Seq(1, 2).map { x =>
      |  val y = x * 2
      |  y + 1/*<# ^map #>*/
      |}
      |""".stripMargin
  )
}
