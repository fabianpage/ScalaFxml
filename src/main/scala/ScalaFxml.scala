


//import scalaz._
//import scalaz.std.string._
//import scalaz.syntax.std._
//import scalaz.std.string._
//import Scalaz._


package com.github.nuriaion

case class Element(label: String, attr:Seq[(String, String)] = Nil, sub:Seq[(String, Seq[Element])] = Nil) {
  //val name:String = attr.find{case (id, _) => id == "fx:id"}.getOrElse(("",label + "_" + scala.util.Random.alphanumeric))._2
  //println(s"label: $label")
  if (label.contains("PCDATA")) {
    println(s"Strange Element!!!!!: $this")
  }
  val name:String = {
    val tmp: Option[(String, String)] = attr.find{case (id, _) => id == "fx:id"}
    val tmp2: (String, String) = tmp.getOrElse(("Z", "gen_" + label + "_" + scala.util.Random.alphanumeric ))
    //println(s"tmpppp: $tmp, $tmp2")
    //"HUHUHU"
    tmp2._2
  }
}

trait ScalaFxmlReader {
  import xml.Node


  def parse(fxml:String):xml.Elem = {
    val x: xml.Elem = scala.xml.XML.loadString(fxml)
    x
  }

  def simplifyXml(s:xml.Node):Element = {
    //WTF is #PCDATA?
    val childs: Seq[(String, Seq[Node])] = s.child.map{(c: Node) => (c.label,c.child)}.filter(!_._1.contains("#PCDATA"))
    println(s"childs: $childs")
    Element(s.label, s.attributes.asAttrMap.toList, childs.map(c => c.copy(_2 = c._2.map(simplifyXml(_)))/*s.child.map(simplifyXml(_)*/))
  }
}

trait ScalaFxmlTranslator {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  import scalaz.std.string._

  def genAttribute[T](id:String, value: T): Tree = REF(id) := LIT(value)
  def genDoublePaneCall(id:String, value: Double): Tree = {
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

  def attr(s:Seq[(String, String)]): Seq[Tree] = {
    val doubleNames = List("height", "width")

    def checkString(s:String, subStrings:Seq[String]):Boolean = {
      !subStrings.map(sub => s.toLowerCase().contains(sub)).filter(b => b).isEmpty
    }
    def doubleAttr(value:String):Option[Double] = parseDouble(value).toOption
    def alignmentAttr(value:String):Boolean = {
      val alignmentNames = List("CENTER")
      alignmentNames.contains(value)
    }
    def translate(id:String, value: String):Option[Tree] = {
      id match {
        case id if (checkString(id, doubleNames) && doubleAttr(value).isDefined) => Some(genAttribute(id, doubleAttr(value).get))
        case id if (id.contains("Pane") && id.contains(".") && doubleAttr(value).isDefined) => Some(genDoublePaneCall(id, doubleAttr(value).get))
        case id if (id.contains("Pane") && id.contains(".") && alignmentAttr(value)) => Some(genAlignmentPaneCall(id, value))
        case _ => None
      }

    }
    s.map(e => translate(e._1, e._2)).filter(_.isDefined).map(_.get)
  }


  def genChildrensRef(sub:Seq[String]):Tree = {
    (REF("content") := LIST(sub.map(REF(_))):Tree)
  }

  def genChildrenRef(name:String, sub:String):Tree = {
    (REF(name) := REF(sub):Tree)
  }

  def subElement(name:String, sub:Seq[String]):Option[Tree] = {
    val singles = List("top", "bottom", "left", "right")
    (name, sub) match {
      case ("children", sub) => Some(genChildrensRef(sub))
      case (name, s :: Nil) if(singles.contains(name)) => Some(genChildrenRef(name, s))
      case _ => None
    }
  }

  def content(sub:Seq[(String,Seq[Element])]):Seq[Option[Tree]] = {
    println(s"content")
    println(s"sub: $sub")
    sub.map{_ match {
      case ("children", e) => Some(genChildrensRef(e.map(_.name)))
    }}
    /*if(!e.sub.isEmpty) {
      val tmp: Seq[String] = e.sub.map {_.name}
      println(s"tmp: $tmp")
      val tmp2: Seq[Tree] = tmp.map(REF(_))
      println(s"tmp2: $tmp2")
      Some(REF("content") := ListClass APPLY (tmp2))
    } else {
      None
    }                                                            */
    //None
  }

  def unroll(e:Element):Seq[Tree] = {

    Seq(
      VAL(e.name) := NEW(ANONDEF(e.label) := BLOCK(
        attr(e.attr) ++ content(e.sub).map(_.get).toList
      ))
    )

  }

  def borderPlate(s:String, e:Element):Tree = {
    TRAITDEF(s) := BLOCK (
      unroll(e)
    )
  }
}

object ScalaFxml extends App with ScalaFxmlReader with ScalaFxmlTranslator{




  /*trait Attribute[T] {
    type T
    def id: String
    def value: T
    def tree: Tree
  }
  case class IntAttribute(id: String, value: Int) extends Attribute {
    val tree = REF(id) := LIT(value)
  }*/

  /*

  def parseAttribute(attr:List[(String, String)]): List[Nothing] = {
    def p(id:String, value:String):Option[Tree] = {
      (id, value) match {
        case (i,v) if(i.toLowerCase.contains("height")) => {
          Some(genAttribute(id,value))
        }
        case _ => None
      }
    }
    //val temp: List[Option[Tree]] = attr.map(case (i,v) => parseAttribute(i,v))
    //temp
    Nil
  }

  case class Attribute[T](id: String, value: Int) {
    val tree = REF(id) := LIT(value)
  }

  case class NodeN(_name: Option[String], klass:String, attr:List[Attribute[_]], childs:List[NodeN])*/




  val fxml: String = """<?xml version="1.0" encoding="UTF-8"?>
<?import java.lang.*?>
<?import java.util.*?>
<?import javafx.scene.control.*?>
<?import javafx.scene.layout.*?>
<?import javafx.scene.paint.*?>
<AnchorPane fx:id="rootPane" maxHeight="-Infinity" maxWidth="-Infinity" minHeight="-Infinity" minWidth="-Infinity" AnchorPane.bottomAnchor="0.0" prefHeight="400.0" prefWidth="600.0" xmlns:fx="http://javafx.com/fxml">
  <children>
    <BorderPane fx:id="theBorderStuff" prefHeight="400.0" prefWidth="600.0" AnchorPane.bottomAnchor="0.0" AnchorPane.leftAnchor="0.0" AnchorPane.rightAnchor="0.0" AnchorPane.topAnchor="0.0">
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

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  val pars = parse(fxml)
  println(s"pars:\n$pars")

  val sim = simplifyXml(pars)
  // wo kommt das #pcdata her?
  println(s"sim:\n$sim")

  println("Hallo")
  println(s"atr:\n${attr(sim.attr).map(treeToString(_))}")

  println("\n\n\n")
  println(treeToString(borderPlate("ScalaFxml", sim)))
}
