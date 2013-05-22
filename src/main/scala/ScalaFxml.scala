


//import scalaz._
//import scalaz.std.string._
//import scalaz.syntax.std._
//import scalaz.std.string._
//import Scalaz._


package com.github.nuriaion

trait ScalaFxmlElement {

  def genId(attributes:Seq[(String, String)]):String = {
    val tmp: Option[(String, String)] = attributes.find{case (id, _) => id == "fx:id"}
    val tmp2: (String, String) = tmp.getOrElse(("", "generatedId"+ scala.util.Random.alphanumeric.take(10).mkString ))
    println("tmp2: " + tmp2)
    tmp2._2
  }

  object Element {
    def apply(klass: String,
              attributes:Seq[(String, String)],
              subElements:Seq[(String, Seq[Element])]):Element =
      Element(klass, genId(attributes), attributes, subElements)
  }
  case class Element(klass: String, id:String, attributes:Seq[(String, String)], subElements:Seq[(String, Seq[Element])])
}

trait ScalaFxmlReader { self: ScalaFxmlElement =>
  import xml._

  def parse(fxml:String):xml.Elem = {
    val x: xml.Elem = scala.xml.XML.loadString(fxml)
    println("xml: " + x)
    x
  }

  def xmlToElement(s:xml.Node):Element = {
    def filterElem: (Node) => Boolean = {
      _ match {
        case e: Elem => true
        case _ => false
      }
    }

    val childs: Seq[Node] = s.child.filter { filterElem}

    val label = s.label
    val attr = s.attributes.asAttrMap.toList
    val subElements: Seq[(String, Seq[Element])] = childs.map{ c =>
      (c.label, c.child.filter{filterElem}.map {e => xmlToElement(e)})
    }

    Element(label, attr, subElements)
  }
}

trait ScalaFxmlTranslator { self: ScalaFxmlElement =>

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  import scalaz.std.string._

  def genAttribute[T](id:String, value: T): Tree = REF(id) := LIT(value)
  def genPaneCall[T](id:String, value: T): Tree = {
    val (klass, function) = splitKlassFunction(id)
    (REF(klass) DOT(generateSetter(function)) APPLY (THIS, LIT(value)))
  }


  def generateSetter(function: String): String = {
    "set" + function.head.toUpper + function.tail
  }

  def splitKlassFunction(id: String): (String,String) = {
    val split: Array[String] = id.split('.')
    val klass = split(0)
    val function = split(1)
    (klass, function)
  }

  def genAlignmentPaneCall(id:String, value: String):Tree = {
    val (klass, function) = splitKlassFunction(id)
    (REF(klass) DOT(generateSetter(function)) APPLY (THIS, REF("Pos") DOT(value)))
  }

  abstract class NaNa {
    def genUnapply(names:List[String]):(String => Option[String]) = { s =>
      if(checkString(s, names)) Some(s) else None
    }

    def checkString(s:String, subStrings:Seq[String]):Boolean = {
      !subStrings.map(sub => s.toLowerCase().contains(sub.toLowerCase())).filter(b => b).isEmpty
    }
  }

  object DoubleName extends NaNa{
    def unapply(x: String):Option[String] = genUnapply(List("height", "width"))(x)
  }

  object BoolName extends NaNa{
    def unapply(x: String):Option[String] = genUnapply(List("mnemonicParsing"))(x)
  }

  object StringName extends NaNa{
    def unapply(x: String):Option[String] = genUnapply(List("text", "style"))(x)
  }

  object AlignmentPane {
    def unapply(x: String):Option[String] = {
      val l = x.toLowerCase
      if(l.contains("pane") && l.contains(".") && l.contains("alignment")) Some(x) else None
    }
  }

  object DoublePane {
    def unapply(x: String):Option[String] = {
      val l = x.toLowerCase
      if(l.contains("pane") && l.contains(".")) Some(x) else None
    }
  }

  object D {
    def unapply(x: String):Option[Double] =
      if(!x.toLowerCase.contains("inf")) parseDouble(x).toOption else None
  }

  object Bool {
    def unapply(x: String):Option[Boolean] = parseBoolean(x).toOption
  }

  object AlignmentAttr {
    def unapply(x: String): Option[String] = {
      val alignmentNames = List("CENTER")
      if(alignmentNames.contains(x)) Some(x) else None
    }
  }

  // Good Solution?
  // Maybe some reflection on scalafx/javafx would be better?
  def attr(s:Seq[(String, String)]): Seq[Tree] = {

    def translate(identifier:String, value: String):Option[Tree] = {
      (identifier, value) match {
        case (DoubleName(id), D(d)) => Some(genAttribute(id, d))
        case (StringName(id), s) => Some(genAttribute(id, s))
        case (BoolName(id), Bool(b))  => Some(genAttribute(id, b))
        case (DoublePane(id), D(d)) => Some(genPaneCall(id, d))
        case (AlignmentPane(id), AlignmentAttr(a)) => Some(genAlignmentPaneCall(id, a))
        case _ => None
      }

    }
    s.map(e => translate(e._1, e._2)).flatten
  }

  object ListSubElement {
    val names: Map[String, String] = Map("children" -> "content")
    def unapply(x: String):Option[String] = names.get(x)
  }

  object SingleSubElement {
    val singles = List("top", "bottom", "left", "right", "center")
    val singlesMap = singles.zip(singles).toMap
    def unapply(x: String):Option[String] = singlesMap.get(x)
  }

  def genChildrensRef(name: String, sub:Seq[String]):Tree = {
    (REF(name) := LIST(sub.map(REF(_))):Tree)
  }

  def genChildrenRef(name:String, sub:String):Tree = {
    (REF(name) := REF(sub):Tree)
  }

  def subElement(name:String, sub:Seq[String]):Option[Tree] = {
    (name, sub) match {
      case (ListSubElement(n), sub) => Some(genChildrensRef(n, sub))
      case (SingleSubElement(n), s :: Nil) => {
        Some(genChildrenRef(n, s))
      }
      case _ => None
    }
  }

  def genSubElementCalls(subElements:Seq[(String, Seq[Element])]):Seq[Tree] = {
    subElements.map(e => subElement(e._1, e._2.map(_.id))).flatten
  }

  def genAttribute(attributes:Seq[(String, String)]):Seq[Tree] = {
    attr(attributes)
  }

  def generateElementCode(e:Element):Tree = {
    VAL(e.id) := NEW(ANONDEF(e.klass) := BLOCK(
      genSubElementCalls(e.subElements) ++ genAttribute(e.attributes)
    ))
  }

  def generateElementTreeCode(elements:Seq[Element]):Seq[Tree] = {
    elements match {
      case e :: xs => generateElementTreeCode(e.subElements.map(_._2).flatten ++ xs) :+ generateElementCode(e)
      case _ => Nil
    }
  }


  def generateCode(pkg: String, klass:String, imports:Seq[String], e:Element):Tree = {
    PACKAGE(pkg) := BLOCK (
      TRAITDEF(klass) := BLOCK (
        imports.map(IMPORT(_)) ++ generateElementTreeCode(Seq(e))
      )
    )
  }

  def generateScalaSource(pkg:String, klass:String, imports:List[String], e:Element): String = {
    treeToString(generateCode(pkg, klass, imports, e))
  }

  val imports = List("scalafx.application.JFXApp",
    "scalafx.application.JFXApp.PrimaryStage",
    "scalafx.geometry.Orientation",
    "scalafx.geometry.Pos",
    "scalafx.scene.control.Label",
    "scalafx.scene.control.TextArea",
    "scalafx.scene.control.Button",
    "scalafx.scene.control.SplitPane",
    "scalafx.scene.layout.Priority",
    "scalafx.scene.layout.BorderPane",
    "scalafx.scene.layout.AnchorPane",
    "scalafx.scene.Scene")

}

object ScalaFxml extends ScalaFxmlReader with ScalaFxmlTranslator with ScalaFxmlElement {}

object ScalaFxmlApp extends App{

  import ScalaFxml._

  val fxml: String = """<?xml version="1.0" encoding="UTF-8"?>
<?import java.lang.*?>
<?import java.util.*?>
<?import javafx.scene.control.*?>
<?import javafx.scene.layout.*?>
<?import javafx.scene.paint.*?>
<AnchorPane fx:id="rootPane" maxHeight="-Infinity" maxWidth="-Infinity" minHeight="-Infinity" minWidth="-Infinity" AnchorPane.bottomAnchor="0.0" prefHeight="400.0" prefWidth="600.0" xmlns:fx="http://javafx.com/fxml">
  <children>
    <BorderPane prefHeight="400.0" prefWidth="600.0" AnchorPane.bottomAnchor="0.0" AnchorPane.leftAnchor="0.0" AnchorPane.rightAnchor="0.0" AnchorPane.topAnchor="0.0">
      <center>
        <Button fx:id="theButton" mnemonicParsing="false" text="Button" />
      </center>
      <top>
        <Label fx:id="theTop" text="Top!" BorderPane.alignment="CENTER" />
      </top>
    </BorderPane>
  </children>
</AnchorPane>
                     """


  val pars: xml.Elem = parse(fxml)

  val sim = xmlToElement(pars)

  val imports = List("scalafx.application.JFXApp",
    "scalafx.application.JFXApp.PrimaryStage",
    "scalafx.geometry.Orientation",
    "scalafx.geometry.Pos",
    "scalafx.scene.control.Label",
    "scalafx.scene.control.TextArea",
    "scalafx.scene.control.Button",
    "scalafx.scene.control.SplitPane",
    "scalafx.scene.layout.Priority",
    "scalafx.scene.layout.BorderPane",
    "scalafx.scene.layout.AnchorPane",
    "scalafx.scene.Scene")

  println(generateScalaSource("FxmlFiles", "simple", imports, sim))



}
