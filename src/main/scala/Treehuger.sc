import treehugger.forest._
import definitions._
import treehuggerDSL._
import scala.languageFeature.postfixOps
val tree: Tree = (Predef_println APPLY LIT("Hello, world!"))

treeToString(tree)
object sym {
  val IntQueue = RootClass.newClass("IntQueue")
  val BasicIntQueue = RootClass.newClass("BasicIntQueue")
  val buf = BasicIntQueue.newValue("buf")
}
val iq = CLASSDEF(sym.IntQueue) withFlags(Flags.ABSTRACT) := BLOCK(
  DEF("get", IntClass),
  DEF("put") withParams(PARAM("x", IntClass))
)





treeToString(iq)




val biq = CLASSDEF(sym.BasicIntQueue) withParents(sym.IntQueue) := BLOCK(
  VAL(sym.buf) withFlags(Flags.PRIVATE) :=
    NEW(ArrayBufferClass TYPE_OF IntClass),
  DEF("get", IntClass) := REF(sym.buf) DOT "remove" APPLY(),
  DEF("put") withParams(PARAM("x", IntClass)) := BLOCK(
    REF(sym.buf) INFIX("+=") APPLY REF("x")
  )
)







treeToString(biq)







object sym2 {
  val BasicIntQueue = RootClass.newClass("BasicIntQueue")
  val buf = BasicIntQueue.newValue("buf")
  val A = ArrowAssocClass.newTypeParameter("A")
  val arrow = ArrowAssocClass.newMethod("->")
  val B = arrow.newTypeParameter("B")
  val T = BasicIntQueue.newAliasType("T")
}

sym2.BasicIntQueue
sym2.buf
sym2.A
sym2.arrow
sym2.B
sym2.T

VAL("foo", IntClass)

VAL("foo", "Int")

TYPE_ARRAY(StringClass)
TYPE_REF(REF("board") DOT "Coord")

treeToString(LIT(1))
treeToString(LIT(1.23F))
treeToString(LIT('H'))
treeToString(LIT("H"))
treeToString(LIT('H))

treeToString(LIT(2) withComment("comments are useful",
  "only if they provide more info than the code"))



treeToString(
  DEF("x") := LIT(0)) withDoc(
  "does something",
  DocTag.See(IntClass)
  )




LIST(Nil)


