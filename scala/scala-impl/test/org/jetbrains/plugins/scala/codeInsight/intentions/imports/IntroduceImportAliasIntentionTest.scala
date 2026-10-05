package org.jetbrains.plugins.scala.codeInsight.intentions.imports

import com.intellij.codeInsight.template.TemplateManager
import com.intellij.codeInsight.template.impl.{TemplateManagerImpl, TemplateState}
import com.intellij.openapi.command.CommandProcessor
import org.jetbrains.plugins.scala.codeInsight.intentions.ScalaIntentionTestBase
import org.jetbrains.plugins.scala.{LatestScalaVersions, ScalaBundle, ScalaVersion}
import org.junit.Assert.{assertNotNull, assertTrue, fail}

abstract class IntroduceImportAliasIntentionTestBase extends ScalaIntentionTestBase {
  override def familyName: String = ScalaBundle.message("family.name.introduce.import.alias")

  def testNotAvailableForLocalDefinition(): Unit = checkIntentionIsNotAvailable(
    s"""object A {
       |  val local = 1
       |  println(${CARET}local)
       |}""".stripMargin
  )

  def testNotAvailableForFullyQualifiedReference(): Unit = checkIntentionIsNotAvailable(
    s"""object A {
       |  val b = scala.collection.mutable.${CARET}ListBuffer.empty[Int]
       |}""".stripMargin
  )

  def testNotAvailableForSamePackageDefinition(): Unit = checkIntentionIsNotAvailable(
    s"""class Foo
       |
       |object A {
       |  val f = new ${CARET}Foo
       |}""".stripMargin
  )

  def testNotAvailableOnImportQualifier(): Unit = checkIntentionIsNotAvailable(
    s"""import scala.collection.${CARET}mutable.ListBuffer
       |
       |object A {
       |  val b = ListBuffer.empty[Int]
       |}""".stripMargin
  )
}

class IntroduceImportAliasIntentionTest extends IntroduceImportAliasIntentionTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_2_13

  def testSingleImportFromUsage(): Unit = doTest(
    s"""import scala.collection.mutable.ListBuffer
       |
       |object A {
       |  val b: ListBuffer[Int] = ${CARET}ListBuffer.empty[Int]
       |  val f: ListBuffer[String] => Unit = _ => ()
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ListBuffer => ListBuffer1}
       |
       |object A {
       |  val b: ListBuffer1[Int] = ListBuffer1.empty[Int]
       |  val f: ListBuffer1[String] => Unit = _ => ()
       |}""".stripMargin
  )

  def testSingleImportFromImport(): Unit = doTest(
    s"""import scala.collection.mutable.${CARET}ListBuffer
       |
       |object A {
       |  val b = new ListBuffer[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ListBuffer => ListBuffer1}
       |
       |object A {
       |  val b = new ListBuffer1[Int]
       |}""".stripMargin
  )

  def testSelectorGroup(): Unit = doTest(
    s"""import scala.collection.mutable.{ArrayBuffer, ListBuffer}
       |
       |object A {
       |  val a = ArrayBuffer.empty[Int]
       |  val b = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ArrayBuffer, ListBuffer => ListBuffer1}
       |
       |object A {
       |  val a = ArrayBuffer.empty[Int]
       |  val b = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testSelectorGroupFromImport(): Unit = doTest(
    s"""import scala.collection.mutable.{ArrayBuffer, ${CARET}ListBuffer}
       |
       |object A {
       |  val b = ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ArrayBuffer, ListBuffer => ListBuffer1}
       |
       |object A {
       |  val b = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testWildcardImport(): Unit = doTest(
    s"""import scala.collection.mutable._
       |
       |object A {
       |  val a = ArrayBuffer.empty[Int]
       |  val b: ListBuffer[Int] = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ListBuffer => ListBuffer1, _}
       |
       |object A {
       |  val a = ArrayBuffer.empty[Int]
       |  val b: ListBuffer1[Int] = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testWildcardInSelectorGroup(): Unit = doTest(
    s"""import scala.collection.mutable.{ArrayBuffer => AB, _}
       |
       |object A {
       |  val a = AB.empty[Int]
       |  val b = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ArrayBuffer => AB, ListBuffer => ListBuffer1, _}
       |
       |object A {
       |  val a = AB.empty[Int]
       |  val b = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testImportedMember(): Unit = doTest(
    s"""import scala.math.Pi
       |
       |object A {
       |  val x = ${CARET}Pi * Pi
       |}""".stripMargin,
    s"""import scala.math.{Pi => Pi1}
       |
       |object A {
       |  val x = Pi1 * Pi1
       |}""".stripMargin
  )

  def testJavaClass(): Unit = doTest(
    s"""import java.util.concurrent.TimeUnit
       |
       |object A {
       |  val f: TimeUnit => Long = _.toMillis(1)
       |  f(${CARET}TimeUnit.SECONDS)
       |}""".stripMargin,
    s"""import java.util.concurrent.{TimeUnit => TimeUnit1}
       |
       |object A {
       |  val f: TimeUnit1 => Long = _.toMillis(1)
       |  f(TimeUnit1.SECONDS)
       |}""".stripMargin
  )

  def testSuggestedNameAvoidsConflicts(): Unit = doTest(
    s"""import scala.collection.mutable.ListBuffer
       |
       |object A {
       |  val ListBuffer1 = 1
       |  val b = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ListBuffer => ListBuffer2}
       |
       |object A {
       |  val ListBuffer1 = 1
       |  val b = ListBuffer2.empty[Int]
       |}""".stripMargin
  )

  def testOnlyUsagesOfThisImportAreRenamed(): Unit = doTest(
    s"""import scala.collection.mutable.ListBuffer
       |
       |object A {
       |  val b = ${CARET}ListBuffer.empty[Int]
       |  val c = scala.collection.mutable.ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ListBuffer => ListBuffer1}
       |
       |object A {
       |  val b = ListBuffer1.empty[Int]
       |  val c = scala.collection.mutable.ListBuffer.empty[Int]
       |}""".stripMargin
  )

  def testNotAvailableForAliasedImport(): Unit = checkIntentionIsNotAvailable(
    s"""import scala.collection.mutable.{ListBuffer => LB}
       |
       |object A {
       |  val b = ${CARET}LB.empty[Int]
       |}""".stripMargin
  )

  def testNotAvailableOnAliasedSelector(): Unit = checkIntentionIsNotAvailable(
    s"""import scala.collection.mutable.{${CARET}ListBuffer => LB}
       |
       |object A {
       |  val b = LB.empty[Int]
       |}""".stripMargin
  )

  def testInplaceRenameTemplate(): Unit = {
    TemplateManagerImpl.setTemplateTesting(getTestRootDisposable)

    myFixture.configureByText(fileType,
      s"""import scala.collection.mutable.ListBuffer
         |
         |object A {
         |  val b: ListBuffer[Int] = ${CARET}ListBuffer.empty[Int]
         |}""".stripMargin
    )
    val intention = findIntentionByName(familyName).getOrElse(fail("Intention is not found").asInstanceOf[Nothing])
    // `startInWriteAction` is false, so the IDE invokes it in a command but outside a write action
    CommandProcessor.getInstance().executeCommand(getProject, (() => intention.invoke(getProject, getEditor, getFile)): Runnable, "Test Intention", null)

    val templateManager = TemplateManager.getInstance(getProject)
    assertNotNull("Expected an active in-place rename template", templateManager.getActiveTemplate(getEditor))

    val state: TemplateState = TemplateManagerImpl.getTemplateState(getEditor)
    val range = state.getCurrentVariableRange
    getEditor.getSelectionModel.setSelection(range.getStartOffset, range.getEndOffset)
    myFixture.`type`("Buf")
    assertTrue("Expected template to finish", templateManager.finishTemplate(getEditor))

    myFixture.checkResult(
      s"""import scala.collection.mutable.{ListBuffer => Buf}
         |
         |object A {
         |  val b: Buf[Int] = Buf.empty[Int]
         |}""".stripMargin
    )
  }
}

class IntroduceImportAliasIntentionTest_Scala3 extends IntroduceImportAliasIntentionTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version >= LatestScalaVersions.Scala_3

  def testSingleImport(): Unit = doTest(
    s"""import scala.collection.mutable.ListBuffer
       |
       |object A {
       |  val b: ListBuffer[Int] = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.ListBuffer as ListBuffer1
       |
       |object A {
       |  val b: ListBuffer1[Int] = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testSelectorGroup(): Unit = doTest(
    s"""import scala.collection.mutable.{ArrayBuffer, ListBuffer}
       |
       |object A {
       |  val a = ArrayBuffer.empty[Int]
       |  val b = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ArrayBuffer, ListBuffer as ListBuffer1}
       |
       |object A {
       |  val a = ArrayBuffer.empty[Int]
       |  val b = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testWildcardImport(): Unit = doTest(
    s"""import scala.collection.mutable.*
       |
       |object A {
       |  val b = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.{ListBuffer as ListBuffer1, *}
       |
       |object A {
       |  val b = ListBuffer1.empty[Int]
       |}""".stripMargin
  )

  def testNotAvailableForAliasedImport(): Unit = checkIntentionIsNotAvailable(
    s"""import scala.collection.mutable.ListBuffer as LB
       |
       |object A {
       |  val b = ${CARET}LB.empty[Int]
       |}""".stripMargin
  )
}

class IntroduceImportAliasIntentionTest_Scala2_XSource3 extends IntroduceImportAliasIntentionTestBase {
  override protected def supportedIn(version: ScalaVersion): Boolean = version == LatestScalaVersions.Scala_2_13

  override protected def additionalCompilerOptions: Seq[String] = Seq("-Xsource:3")

  def testSingleImport(): Unit = doTest(
    s"""import scala.collection.mutable.ListBuffer
       |
       |object A {
       |  val b = ${CARET}ListBuffer.empty[Int]
       |}""".stripMargin,
    s"""import scala.collection.mutable.ListBuffer as ListBuffer1
       |
       |object A {
       |  val b = ListBuffer1.empty[Int]
       |}""".stripMargin
  )
}
