import treehugger.forest._
import definitions._
import treehuggerDSL._

object SimpleTreehuger extends App {

  object sym {
    val SimpleFxml = RootClass.newClass("SimpleFxml")
  }

  def assign[T](id:String, value:T):Tree = {
    //val sym =
    REF(id) := LIT(value)
  }

  def assign(id:String, value:Tree): Tree = {
    REF(id) := value
  }

  def cont(c:List[Element]): Tree = {
    val elems:List[Tree] = c.map{e => REF(e.name)}
    LIST(elems:_*)
  }

  case class Element(_name: Option[String], klass:String, atr: List[Tree], content: List[Element]) {
    val name = _name.getOrElse(klass + "HUHU")
    def tree:Tree = {
        val contents:List[Tree] = if(content == Nil) atr else atr :+ (REF("content") := cont(content))
        VAL(name) := NEW(ANONDEF(klass) := BLOCK(
          ((contents):_*)
        ))
    }

    def unroll:List[Tree] = {
      content.flatMap(_.unroll) :+ tree
    }

  }

  val text = "text"
  val simpleFxml:Tree = TRAITDEF(sym.SimpleFxml) := BLOCK (
    VAL("theButton") := NEW(ANONDEF("Button") := BLOCK(
      assign("id", "theButton"),
      assign("mnemonicParsing", FALSE),
      assign(text, "Button"),
      assign("style", "-fx-background-color: green")

    ))
    ,
    VAL("theButton2") := LIT(0),
    DEF("philosophize") := BLOCK(
      LIT(0)
    )
  )

  println("s:")

  println(simpleFxml)

  println(treeToString(simpleFxml))

  println("a:")

  println(treeToString(Element(
    Some("theButton"),
    "Button",
    List(assign("id","theButton")),
    Nil
  ).tree))


  val unroll = Element(
    Some("topPane"),
    "AnchorPane",
    List(assign("id", "topPane"),
      assign("prefHeight", 400)),
    List(Element(
      Some("theBorderStuff"),
      "BorderPane",
      List(assign("id", "theBorderStuff"),
        assign("style", "-fx-background-color: black")),
      Nil),
      Element(
        Some("theBorderStuff2"),
        "BorderPane",
        List(assign("id", "theBorderStuff"),
          assign("style", "-fx-background-color: black")),
        Nil)
    )
  ).unroll

  println(treeToString(BLOCK(unroll)))

  println(treeToString(cont(List(Element(
    Some("theBorderStuff"),
    "BorderPane",
    List(assign("id", "theBorderStuff"),
      assign("style", "-fx-background-color: black")),
    Nil),
    Element(
      Some("theBorderStuff2"),
      "BorderPane2",
      List(assign("id", "theBorderStuff"),
        assign("style", "-fx-background-color: black")),
      Nil)
  ))))
}