package com.github.nuriaion.test

import com.github.nuriaion.{ScalaFxmlElement, ScalaFxmlTranslator, ScalaFxmlReader}
import org.jcp.xml.dsig.internal.dom.DOMTransform
import org.specs2.matcher.MatchResult

class ScalaFxmlSpec extends org.specs2.Specification with ScalaFxmlTranslator with ScalaFxmlElement { def is =
  "subElement calls" ^
  "top" ! checkGenerateSubElementLink("top", "theTopElement") ^
  "bottom" ! checkGenerateSubElementLink("bottom", "theTopElement") ^
  "left" ! checkGenerateSubElementLink("left", "theTopElement") ^
  "right" ! checkGenerateSubElementLink("right", "theTopElement") ^
  "center" ! checkGenerateSubElementLink("center", "theTopElement") ^
  "columns" ! checkGenerateSubElementLink("columns", List("someColumns"), "items") ^
  "menus" ! checkGenerateSubElementLink("menus", List("someColumns")) ^
  "expandedPane" ! checkGenerateSubElementLink("expandedPane", "asdf") ^
  "panes" ! checkGenerateSubElementLink("panes", List("someColumns")) ^
  "items" ! checkGenerateSubElementLink("items", List("someColumns")) ^
  "content" ! checkGenerateSubElementLink("content", "someColumns") ^
  "children" ! checkGenerateSubElementLink("children", List("someElement"), "content") ^
  "childrens" ! checkGenerateSubElementLink("children", List("someElement", "someOtherElement"), "content") ^
  "no call" ! checkGenerateNoSubElement("huhu") ^
  p^
  "attribute generation" ^
  br^
  "Double" ^
  "height" ! checkAttribute("prefHeight", 412.3) ^
  "width" ! checkAttribute("prefWidth", 300.0) ^
  "layoutX" ! checkAttribute("layoutX", 33.5) ^
  "-Infinity" ! checkNoAttribute("minHeight", "-Infinity") ^
  "-Infinity" ! checkNoAttribute("minWidth", "-Infinity") ^
  br^ bt^ "String" ^
  "text" ! checkAttribute("text", "someText") ^
  "style" ! checkAttribute("style", "someStyle") ^
  "fx:id" ! checkNoAttribute("fx:id", "someId") ^
  br^ bt^  "Boolean" ^
  "mnemonicParsing" ! checkAttribute("mnemonicParsing", false) ^
  "wrapText" ! checkAttribute("wrapText", true) ^
  br^ bt^ "Priority" ^
  "hgrow" ! checkPriorityAttribute("hgrow", "SOMETIMES") ^
  "vgrow" ! checkPriorityAttribute("vgrow", "SOMETIMES") ^
  endp^
  p^
  "Single Element Parser" ^
  "a Button" ! checkButton ^
  "a Border Pane" ! checkBorderPane ^
  endp^
  p^
  "Element Tree Parser" ^
  "a Pane with a Button" ! elementTree ^
  endp^
  end



  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  def checkNoAttribute[T](id:String, value:T) = {
    (attr(Seq((id, value.toString))):Seq[Tree]).map(treeToString(_)) === Nil
  }

  def checkAttribute[T](id:String, value:T) = {

    val attribute:Tree = REF(id) := LIT(value)
    genAttribute(Seq((id, value.toString))).map(treeToString(_)) === Seq(attribute).map(treeToString(_))
  }

  def checkPriorityAttribute(id:String, value:String) = {
    val code:Tree = REF(id) := (REF("Priority") DOT value)
    genAttribute(Seq((id, value))).map(treeToString(_)) === Seq(code).map(treeToString(_))
  }

  def checkGenerateNoSubElement(fxmlPane:String) = {
    val subElements = Seq(
      (fxmlPane,
        Seq(Element("", "", Nil, Nil))))
    genSubElementCalls(subElements) === Nil
  }

  def checkGenerateSubElementLink(fxmlPane: String, elementId:Seq[String]): MatchResult[Any] =
    checkGenerateSubElementLink(fxmlPane, elementId, fxmlPane)
  def checkGenerateSubElementLink(fxmlPane: String, elementId:Seq[String], scalaPane: String): MatchResult[Any] = {
    val subElements = Seq(
      (fxmlPane,
        elementId.map(Element("someKlassz",
          _,
          Nil,
          Nil))))
    val res:Seq[Tree] = genSubElementCalls(subElements)
    val soll:Seq[Tree] = Seq((REF(scalaPane) := LIST(elementId.map(REF(_)))))
    soll.map(treeToString(_)) === res.map(treeToString(_))
  }

  def checkGenerateSubElementLink(fxmlPane: String, elementId:String): MatchResult[Any] = checkGenerateSubElementLink(fxmlPane, elementId, fxmlPane)
  def checkGenerateSubElementLink(fxmlPane: String, elementId:String, scalaPane: String): MatchResult[Any] = {
    val subElements = Seq(
      (fxmlPane,
        Seq(Element("someKlassz",
                    elementId,
                    Nil,
                    Nil))))
    val res:Seq[Tree] = genSubElementCalls(subElements)
    val soll:Seq[Tree] = Seq((REF(scalaPane) := REF(elementId)))
    soll.map(treeToString(_)) === res.map(treeToString(_))
  }

  val button = Element("Button", "theButton", Seq(("text", "someText")), Nil)
  val borderPane = Element("BorderPane", "aBorderPane",
    Seq(("prefHeight", "400.0"), ("style", "-fx-background-color: black")),
    Seq(("center", Seq(button))))

  def checkButton = {
    val code:Tree =
      VAL("theButton") := NEW(ANONDEF("Button") := BLOCK(
        REF("text") := LIT("someText")
      ))
    treeToString(generateElementCode(button)) === treeToString(code)
  }

  def checkBorderPane = {
    val code:Tree =
      VAL("aBorderPane") := NEW(ANONDEF("BorderPane") := BLOCK(
        REF("center") := REF("theButton"),
        REF("prefHeight") := LIT(400.0),
        REF("style") := LIT("-fx-background-color: black")
      ))
    treeToString(generateElementCode(borderPane)) === treeToString(code)
  }

  def elementTree = {
    val code:Tree =
      PACKAGE("FxmlFiles") := BLOCK (
        TRAITDEF("simple") := BLOCK (
          IMPORT("scalafx.application.JFXApp"),
          IMPORT("scalafx.application.JFXApp.PrimaryStage"),
          IMPORT("scalafx.geometry.Orientation"),
          IMPORT("scalafx.geometry.Pos"),
          IMPORT("scalafx.scene.control.Label"),
          IMPORT("scalafx.scene.control.TextArea"),
          IMPORT("scalafx.scene.control.Button"),
          IMPORT("scalafx.scene.control.SplitPane"),
          IMPORT("scalafx.scene.layout.Priority"),
          IMPORT("scalafx.scene.layout.BorderPane"),
          IMPORT("scalafx.scene.layout.AnchorPane"),
          IMPORT("scalafx.scene.Scene"),
          IMPORT("scalafx.geometry._"),
          VAL("theButton") := NEW(ANONDEF("Button") := BLOCK(
            REF("text") := LIT("someText")
          )),
          VAL("aBorderPane") := NEW(ANONDEF("BorderPane") := BLOCK(
            REF("center") := REF("theButton"),
            REF("prefHeight") := LIT(400.0),
            REF("style") := LIT("-fx-background-color: black")
          ))
        )
      )
    treeToString(generateCode("FxmlFiles", "simple", imports, borderPane)) === treeToString(code)
  }

}


class ScalaFxmlReaderSpec extends org.specs2.Specification with ScalaFxmlReader with ScalaFxmlElement { def is =
  "Parse import statements" ! parseImportStatements ^
  "Parse fxml import statements" ! parseFxmlSource ^
  end


  def parseImportStatements = {
    val fxml = """<?import java.lang.*?>
                 |<?import java.util.*?>
                 |<?import javafx.scene.control.*?>
                 |<?import javafx.scene.layout.*?>
                 |<?import javafx.scene.paint.*?>""".stripMargin

    parseImports(fxml) === List("scalafx.scene.control._",
      "scalafx.scene.layout._",
      "scalafx.scene.paint._")
  }

  def parseFxmlSource = {
    val fxml = """<?xml version="1.0" encoding="UTF-8"?>
                         |<?import java.lang.*?>
                         |<?import java.util.*?>
                         |<?import javafx.scene.control.*?>
                         |<?import javafx.scene.layout.*?>
                         |<?import javafx.scene.paint.*?>
                         |<AnchorPane fx:id="rootPane" maxHeight="-Infinity" maxWidth="-Infinity" minHeight="-Infinity" minWidth="-Infinity" AnchorPane.bottomAnchor="0.0" prefHeight="400.0" prefWidth="600.0" xmlns:fx="http://javafx.com/fxml">
                         |  <children>
                         |    <BorderPane prefHeight="400.0" prefWidth="600.0" AnchorPane.bottomAnchor="0.0" AnchorPane.leftAnchor="0.0" AnchorPane.rightAnchor="0.0" AnchorPane.topAnchor="0.0">
                         |      <center>
                         |        <Button fx:id="theButton" mnemonicParsing="false" text="Button" />
                         |      </center>
                         |      <top>
                         |        <Label fx:id="theTop" text="Top!" BorderPane.alignment="CENTER" />
                         |      </top>
                         |    </BorderPane>
                         |  </children>
                         |</AnchorPane>""".stripMargin

    parseImports(fxml) === List("scalafx.scene.control._",
      "scalafx.scene.layout._",
      "scalafx.scene.paint._")
  }
}