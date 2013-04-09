/**
 * Created with IntelliJ IDEA.
 * User: fabian
 * Date: 26.03.13
 * Time: 16:17
 * To change this template use File | Settings | File Templates.
 */


"ASDFfdsa" match {
  case s => s
}
























































































































































"ASDFfdsa" match {
  case s if s.startsWith("ASDF") => s
}
"9".toInt







































object IntP {
  def unapply(s : String) : Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}
"Start"
"9" match {
  case IntP(i) => i
}













































case class ParseOp[T](op: String => T)



implicit val popDouble = ParseOp[Double](_.toDouble)


implicit val popInt = ParseOp[Int](_.toInt)




val a = List(false, true, false)

"AnchorPane.bottomAnchor".split(".")







