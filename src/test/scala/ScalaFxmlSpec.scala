package com.github.nuriaion.test

import com.github.nuriaion.{ScalaFxmlElement, ScalaFxmlTranslator, ScalaFxmlReader}
import org.jcp.xml.dsig.internal.dom.DOMTransform
import org.specs2.matcher.MatchResult

/**
 * Created with IntelliJ IDEA.
 * User: fabian
 * Date: 26.03.13
 * Time: 15:49
 * To change this template use File | Settings | File Templates.
 */
/**class ScalaFxmlReaderSpec
  extends org.scalatest.FunSpec
  with org.scalatest.matchers.ShouldMatchers
  with ScalaFxmlReader{
  describe("A ScalaFxmlReader") {
    it("should parse a flat xml") {
      val fxml = <AnchorPane ></AnchorPane>
      xmlToElement(fxml) should equal(Element("AnchorPane", Nil, Nil))
    }
    it("should parse some properties") {
      val fxml = <AnchorPane prefHeight="400.0" prefWidth="600.0"></AnchorPane>
      xmlToElement(fxml) should equal(
        Element("AnchorPane",
          Seq(
            ("prefHeight","400.0"),
            ("prefWidth", "600.0")), Nil))
    }
    it("should parse a child") {
      val fxml = <AnchorPane><children><BorderPane></BorderPane></children></AnchorPane>
      xmlToElement(fxml) should equal(Element("AnchorPane", Nil,
        Seq(("children",Seq(Element("BorderPane", Nil, Nil))))))
    }
  }
}   */

/*class ScalaFxmlTranslatorSpec extends FunSpec with ShouldMatchers with ScalaFxmlTranslator {
  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  describe("A Translator") {
    it("should parse some double properties") {
      attr(Nil) should equal(Nil)
      (attr(Seq(("prefHeight", "412.3"))):Seq[Tree]) should equal(
        (Seq(REF("prefHeight") := LIT(412.3)):Seq[Tree])
      )
    }
  }
}               */


class ScalaFxmlReaderSpecS
  extends org.specs2.Specification
  with ScalaFxmlReader with ScalaFxmlElement { def is =
  "A ScalaFxmlReader" ^
  "should parse a flat xml" ! checkFlatXml ^
  "should parse some properties" ! parseSomeProperties ^
  "should parse a child" ! parseAChild ^
  end

  def checkFlatXml = {
    val fxml = <AnchorPane ></AnchorPane>
    xmlToElement(fxml) === (Element("AnchorPane", Nil, Nil))
  }

  def parseSomeProperties =  {
    val fxml = <AnchorPane prefHeight="400.0" prefWidth="600.0"></AnchorPane>
    xmlToElement(fxml) === (
      Element("AnchorPane",
        Seq(
          ("prefHeight","400.0"),
          ("prefWidth", "600.0")), Nil))
  }

  def parseAChild = {
    val fxml = <AnchorPane><children><BorderPane></BorderPane></children></AnchorPane>
    xmlToElement(fxml) === (Element("AnchorPane", Nil,
      Seq(("children",Seq(Element("BorderPane", Nil, Nil))))))
  }
}


class ScalaFxmlTranslatorSpec2 extends org.specs2.Specification with ScalaFxmlTranslator with ScalaFxmlElement { def is =
  "This is a specification to check the ScalaFxmlTranslator" ^
  p^
  "Double properties" ^
  "height" ! checkProperty("prefHeight", 412.3) ^
  "width" ! checkProperty("prefWidth", 300.0) ^
  "-Infinity" ! checkNoProprty("minHeight", "-Infinity") ^
  "-Infinity" ! checkNoProprty("minWidth", "-Infinity") ^
  p^
  "String properties" ^
  "id" ! checkProperty("fx:id", "theButton") ^
  "text" ! checkProperty("text", "someText") ^
  "style" ! checkProperty("style", "someStyle") ^
  p^
  "Boolean properties" ^
  "mnemonicParsing" ! checkProperty("mnemonicParsing", false) ^
  p^
  "Panes" ^
  "AnchorPane" ! checkDoublePane("AnchorPane", "bottomAnchor", 0.0) ^
  "BorderPane" ! checkAlignmentPane("BorderPane", "alignment", "CENTER") ^
  p^
  "Sub-elements" ^
  "children" ! checkSubElements("children", Seq("A", "B")) ^
  "top" ! checkSubElement("top", "c")  ^
  "bottom" ! checkSubElement("bottom", "B")  ^
  "left" ! checkSubElement("left", "h")  ^
  "right" ! checkSubElement("right", "i")  ^
  end



  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def checkNoProprty[T](id:String, value:T) = {
    (attr(Seq((id, value.toString))):Seq[Tree]).map(treeToString(_)) === Nil
  }

  def checkProperty[T](id:String, value:T) = {
    (attr(Seq((id, value.toString))):Seq[Tree]).map(treeToString(_)) ===
      (Seq(REF(id) := LIT(value)):Seq[Tree]).map(treeToString(_))
  }

  def checkDoublePane(klass:String, function:String, value:Double) = {
    (attr(Seq((klass + "." + function, value.toString))):Seq[Tree]).map(treeToString(_)) ===
      (Seq(REF(klass) DOT("set" + function.head.toUpper + function.tail) APPLY (THIS, LIT(value))):Seq[Tree]).map(treeToString(_))
  }

  def checkAlignmentPane(klass:String, function:String, value:String) = {
    (attr(Seq((klass + "." + function, value))):Seq[Tree]).map(treeToString(_)) ===
      (Seq(REF(klass) DOT("set" + function.head.toUpper + function.tail) APPLY (THIS, REF("Pos") DOT(value) )):Seq[Tree]).map(treeToString(_))
  }

  def checkSubElements(name:String, sub:Seq[String]) = {
    subElement(name, sub).map(treeToString(_)) must be_==(
      Some(REF("content") := LIST(sub.map(REF(_))):Tree).map(treeToString(_)))
  }

  def checkSubElement(name:String, sub:String) = {
    subElement(name, Seq(sub)).map(treeToString(_)) must be_==(
      Some(REF(name) := REF(sub):Tree).map(treeToString(_)))
  }

  //def checkAlignmentPane

/*  def heightProperty = {
    /*(attr(Seq(("prefHeight", "412.3"))):Seq[Tree]).map(treeToString(_)) ===
    (Seq(REF("prefHeight") := LIT(412.3)):Seq[Tree]).map(treeToString(_))*/
    checkProperty("prefHeight", 412.3)
  }

  def widthProperty = {
    /*(attr(Seq(("prefWidth", "300")))).map(treeToString(_)) ====
      (Seq(REF("prefWidth") := LIT(300.0))).map(treeToString(_))*/
    checkProperty("prefWidht", 412.3)
  }*/
}
