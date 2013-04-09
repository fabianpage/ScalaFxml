import org.xml.sax.InputSource
import xml.{Node, MetaData, Elem, Source}


object SimpleXml extends App {
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
  val s: InputSource = Source.fromString(fxml)
  val x: Elem = scala.xml.XML.load(s)
  val list: List[MetaData] = x.attributes.toList

  println(x)
  println(list)
  val child: Seq[Node] = x.child
  println(child)
  println(child.head)

  println(x.attributes.get("fx:id"))
  println("HUHU")
  println(x \ "@maxHeight")
  println(x \ "@fx:id")

  println(x \ "@AnchorPane.bottomAnchor")

  case class NodeN(label:String, attr:List[(String, String)], sub:Seq[NodeN])

  val n = NodeN(x.label,x.attributes.asAttrMap.toList,Nil)

  def simplifyXml(s:Node):NodeN = {
    NodeN(s.label, s.attributes.asAttrMap.toList, s.child.map(simplifyXml(_)))
  }

  println(n)

  /*def name(attr:List[MetaData]):Option[String] = {
    attr.map(m => (m.))
  }*/

  import SimpleTreehuger.Element


}
