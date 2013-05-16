


//import scalaz._
//import scalaz.std.string._
//import scalaz.syntax.std._
//import scalaz.std.string._
//import Scalaz._


package com.github.nuriaion

import scala.xml.Elem
import scala.collection.immutable

trait ScalaFxmlElement {
  case class Element(label: String, attr:Seq[(String, String)] = Nil, sub:Seq[(String, Seq[Element])] = Nil) {
    if (label.contains("PCDATA")) {
      println(s"Strange Element!!!!!: $this")
    }
    val name:String = {
      val tmp: Option[(String, String)] = attr.find{case (id, _) => id == "fx:id"}
      val tmp2: (String, String) = tmp.getOrElse(("", "gen_" + label + "_" + scala.util.Random.alphanumeric ))
      tmp2._2
    }
  }
}

trait ScalaFxmlReader { self: ScalaFxmlElement =>
  import xml._



  def parse(fxml:String):xml.Elem = {
    val x: xml.Elem = scala.xml.XML.loadString(fxml)
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
    val stringNames = List("id", "text", "style")
    val booleanNames = List("mnemonicParsing")

    def checkString(s:String, subStrings:Seq[String]):Boolean = {
      !subStrings.map(sub => s.toLowerCase().contains(sub.toLowerCase())).filter(b => b).isEmpty
    }
    def doubleAttr(value:String):Option[Double] = parseDouble(value).toOption
    def booleanAttr(value:String):Option[Boolean] = parseBoolean(value).toOption
    def alignmentAttr(value:String):Boolean = {
      val alignmentNames = List("CENTER")
      alignmentNames.contains(value)
    }

    abstract class NaNa {
      def genUnapply(names:List[String]):(String => Option[String]) = { s =>
        if(checkString(s, names)) Some(s) else None
      }
    }
    object DoubleName extends NaNa{
      def unapply(x: String):Option[String] = genUnapply(doubleNames)(x)//if(checkString(x, doubleNames)) Some(x) else None
    }

    object D {
      def unapply(x: String) = parseDouble(x).toOption
    }

    def translate(id:String, value: String):Option[Tree] = {
      (id, value) match {
        case (DoubleName(id), D(d)) if (checkString(id, doubleNames) ) => Some(genAttribute(id, d))
        case (id, _) if (checkString(id, stringNames)) => Some(genAttribute(id, value))
        case (id, _) if (checkString(id, booleanNames)) => Some(genAttribute(id, booleanAttr(value).get))
        case (id, _) if (id.contains("Pane") && id.contains(".") && doubleAttr(value).isDefined) => Some(genDoublePaneCall(id, doubleAttr(value).get))
        case (id, _) if (id.contains("Pane") && id.contains(".") && alignmentAttr(value)) => Some(genAlignmentPaneCall(id, value))
        case x => {println(s"HUUUUUUUUUUUUU $x");None}
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
    //println(s"content")
    //println(s"sub: $sub")
    if(sub.size > 0) {
      sub.map{_ match {
        case ("children", e) => Some(genChildrensRef(e.map(_.name)))
        case _ => None
      }}
    } else {
      Nil
    }
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

  /*def unroll2(sub:Seq[(String, Seq[Element])]):Seq[Tree] = {
    val tmp1: Seq[Tree] = for{
      s <- sub
      elem <- s._2
    } yield {
      VAL(elem.name) := NEW(ANONDEF(elem.label) := BLOCK(
        //attr(elem.attr)
        attr(elem.attr) //++ content(elem.sub).map(_.get).toList
      ))
    }

    val tmp2: List[List[Tree]] = for {
      s <- sub.toList
      elems <- s._2.toList
    } yield {
      val asdf: Element = elems
      unroll2(elems.sub).toList
    }

    val tmp3: List[Tree] = tmp2.flatten

      //sub.map(elems => elems._2.map(e => unroll2(e)))

    tmp1 ++ tmp3.toSeq
  }*/

  def unroll(e:Element):Seq[Tree] = {
    val tmp1: Seq[treehugger.forest.type#Tree] = content(e.sub).toList.flatten.toSeq
    //val tmp2: List[treehugger.forest.type#Tree] = tmp1.map(_.get).toList
    //val asdf: Seq[Tree] = content(e.sub).flatten
    //println(s"asdf: $asdf")
    val asdf2: Seq[Tree] = attr(e.attr) ++ tmp1
    //val asdf3: Seq[Tree] = asdf ++ asdf2
    val tmp:Seq[Tree] = Seq(VAL(e.name) := NEW(ANONDEF(e.label) := BLOCK(
      //attr(e.attr) ++ content(e.sub).map(_.get).toListasdf
      asdf2// ++ e.sub.map(s=> unroll(s._2))
    )))
    //unroll2(e.sub) ++ tmp
    //asdf2
    subse(e) ++ tmp
  }

  def subs(elem:Element):Seq[Element] =
    for {
      s <- elem.sub
      e:Element <- s._2
    } yield {
      e
    }

  def subse(elem:Element) : Seq[Tree] = {
    for {
      s <- subs(elem)
      e <- unroll(s)
    } yield e
  }

  def borderPlate(s:String, e:Element):Tree = {

    TRAITDEF(s) := BLOCK (
      unroll(e)
    )
  }
}

object ScalaFxml extends App with ScalaFxmlReader with ScalaFxmlTranslator with ScalaFxmlElement{

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

  val pars: Elem = parse(fxml)
  //println(s"pars:\n$pars")

  val sim = xmlToElement(pars)
  // wo kommt das #pcdata her?
  println(s"sim:\n$sim")

  println("Hallo")
  println(s"atr:\n${attr(sim.attr).map(treeToString(_))}")

  println("\n\n\n!!!!!!!!!!!!!!!borderPlate:")
  println(treeToString(borderPlate("ScalaFxml", sim)))



}
