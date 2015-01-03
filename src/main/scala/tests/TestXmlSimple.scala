package tests

/**
 * Created by tim on 30/12/14.
 */
import shapeless._
import shapeless.labelled._
import scala.xml._

trait XmlParser[A] {
  def parse(n: NodeSeq): A
}


object XmlParser {
  def apply[T](implicit st: Lazy[XmlParser[T]]): XmlParser[T] = st.value


  implicit def deriveHNil: XmlParser[HNil] =
    new XmlParser[HNil] {
      def parse(s: NodeSeq) =
        HNil
    }

  implicit def deriveHCons[K <: Symbol, V, T <: HList]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[XmlParser[V]],
   sct: Lazy[XmlParser[T]]
    ): XmlParser[FieldType[K, V] :: T] =
    new XmlParser[FieldType[K, V] :: T] {
      def parse(jv: NodeSeq): FieldType[K, V] :: T = {
        val childish = jv \ key.value.name
        val child = childish.headOption.getOrElse(NodeSeq.Empty)
        println(s"dervieHCons with jv $jv looking for ${key.value.name} found as $childish")
        val answer = {
          val front = scv.value.parse(child)
          println(s"hcons front is $front")
          val back = sct.value.parse(jv)
          println(s"hcons back is $back")
          field[K](front) :: back
        }
        println(s"HCons answer is $answer")
        answer
      }
    }

  implicit def deriveCNil: XmlParser[CNil] = new XmlParser[CNil] {
    def parse(s: NodeSeq): CNil = throw new Exception("no coproduct found")
  }

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[XmlParser[V]],
   sct: Lazy[XmlParser[T]]
    ): XmlParser[FieldType[K, V] :+: T] =
    new XmlParser[FieldType[K, V] :+: T] {
      def parse(jv: NodeSeq): FieldType[K, V] :+: T = {
        val child = (jv \ key.value.name)
        println(s"parse for dervieCCons with $jv looking for ${key.value.name} found as $child")
        val answer = child match {
          case NodeSeq.Empty =>
            println("missed this one, try next")
            Inr(sct.value.parse(jv))
          case nn =>
            println(s"found coproduct ${key.value.name},  now parse it")
            val parsed = scv.value.parse(nn)
            Inl(field[K](parsed))
        }
        println(s"coProduct answer is $answer")
        answer
      }

    }

  implicit def deriveInstance[F, G]
  (implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[XmlParser[G]]): XmlParser[F] =
    new XmlParser[F] {
      def parse(s: NodeSeq): F = {
        val parsed = sg.value.parse(s)
        gen.from(parsed)
      }
    }

}

trait XmlWriter[A] {
  def write(name: Option[String], a: A): NodeSeq
}

object XmlWriter {
  def apply[T](implicit st: Lazy[XmlWriter[T]]): XmlWriter[T] = st.value

  def toNode(name: Option[String], ns: NodeSeq): NodeSeq = name.map(n => Elem(null, n, Null, TopScope, true, ns: _*)).getOrElse(NodeSeq.Empty)
  def toNode(name: Option[String], s: String): NodeSeq = name.map(n => Elem(null, n, Null, TopScope, true, Text(s.trim))).getOrElse(NodeSeq.Empty)
  def toNode(name: Option[String], content: Seq[NodeSeq]): NodeSeq = name.map(n => Elem(null, n, Null, TopScope, true, content.flatten: _*)).getOrElse(NodeSeq.Empty)

  implicit def deriveHNil: XmlWriter[HNil] =
    new XmlWriter[HNil] {
      def write(name: Option[String], n: HNil) = List.empty
    }

  implicit def deriveHCons[K <: Symbol, V, T <: HList]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[XmlWriter[V]],
   sct: Lazy[XmlWriter[T]]
    ): XmlWriter[FieldType[K, V] :: T] =
    new XmlWriter[FieldType[K, V] :: T] {
      def write(name: Option[String], ft: FieldType[K, V] :: T): NodeSeq = {
        println(s"hcons writer name $name key ${key.value.name}")
        val h = scv.value.write(Some(key.value.name), ft.head)
        val t = sct.value.write(None, ft.tail)
        println(s"hcons writer name $name key ${key.value.name} h is $h t is $t")
        // this is too much like magic
        val ht = h ++ t
        val answer = name.map { nm =>
          toNode(name, ht)
        }.getOrElse(ht)
        println(s"hcons answer is $answer")
        answer
      }
    }


  implicit def deriveCNil: XmlWriter[CNil] = new XmlWriter[CNil] {
    def write(name: Option[String], t: CNil) = List.empty
  }

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[XmlWriter[V]],
   sct: Lazy[XmlWriter[T]]
    ): XmlWriter[FieldType[K, V] :+: T] =
    new XmlWriter[FieldType[K, V] :+: T] {
      def write(name: Option[String], lr: FieldType[K, V] :+: T): NodeSeq = {
        println(s"ccons write for ${key.value.name} name $name")
        lr match {
          case Inl(l) => scv.value.write(Some(key.value.name), l)
          case Inr(r) => sct.value.write(Some(key.value.name), r)
        }
      }
    }

  implicit def deriveInstance[F, G]
  (implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[XmlWriter[G]]): XmlWriter[F] =
    new XmlWriter[F] {
      def write(name: Option[String], t: F): NodeSeq = sg.value.write(name, gen.to(t))
    }

}



object XmlSupport2 {
  implicit val IntParser = new XmlParser[Int] {
    def parse(n: NodeSeq) = n.head.text.trim.toInt
  }
  implicit val StringParser = new XmlParser[String] {
    def parse(n:NodeSeq) = n.head.text.trim
  }


  class OptParser[A](implicit aParser: XmlParser[A]) extends XmlParser[Option[A]] {
    def parse(n: NodeSeq): Option[A] = n.headOption.map(no => aParser.parse(no))
  }

  // we need a label, because unlike json you can't have anonymous items in an array - they each need to be a node
  class ListParser[A](label: String)(implicit aParser: XmlParser[A]) extends XmlParser[List[A]] {
    def parse(n: NodeSeq): List[A] = {
      println(s"in ListParser with node $n")
      val children = n \ label
      children.map(aParser.parse(_)).toList
    }
  }
  
  implicit def mkListParser[A](implicit aParser: XmlParser[A]) = new ListParser[A]("item")
  implicit def mkOptParser[A](implicit aParser: XmlParser[A]) = new OptParser[A]

  import XmlWriter._

  implicit val IntWriter = new XmlWriter[Int] {
    def write(name: Option[String], i: Int)  = toNode(name, i.toString)
  }
  implicit val StringWriter = new XmlWriter[String] {
    def write(name: Option[String], s: String)  = toNode(name, s)
  }

  class OptWriter[A](implicit aWriter: XmlWriter[A]) extends XmlWriter[Option[A]] {
    def write(name: Option[String], aOpt: Option[A]) = aOpt.map(a => aWriter.write(name, a)).getOrElse(NodeSeq.Empty)
  }

  class ListWriter[A](label: String)(implicit aWriter: XmlWriter[A]) extends XmlWriter[List[A]] {
    def write(name: Option[String], aList: List[A]): NodeSeq = {
      val items = aList.map(a => toNode(Some(label), aWriter.write(None, a)))
      println(s"list writer writes $items")
      toNode(name, items)
    }
  }
  implicit def mkListWriter[A](implicit aWriter: XmlWriter[A]) = new ListWriter[A]("item")
  implicit def mkOptWriter[A](implicit aWriter: XmlWriter[A]) = new OptWriter[A]


}

object TestXmlSimple extends App {

  import Common._
  import XmlSupport2._

  def parse(n: NodeSeq): Msg = {

    val xmlParser = XmlParser[Msg]
    xmlParser.parse(n)
  }

  val singleWrapped = <Stuff>
    <DinnerIn>
      <target>me</target>
      <menu>
        <first>soup</first>
        <mains>beef</mains>
        <dessert>ice-cream</dessert>
      </menu>
    </DinnerIn>
  </Stuff>

  val optionWrappedNoFriend = <Stuff>
    <DinnerInWith>
      <target>me</target>
      <menu>
        <first>soup</first>
        <mains>beef</mains>
        <dessert>ice-cream</dessert>
      </menu>
    </DinnerInWith>
  </Stuff>
  val optionWrappedFriend = <Stuff>
    <DinnerInWith>
      <target>me</target>
      <menu>
        <first>soup</first>
        <mains>beef</mains>
        <dessert>ice-cream</dessert>
      </menu>
      <friend>fred</friend>
    </DinnerInWith>
  </Stuff>
  val feastNoMenu = <Stuff>
    <Feast>
      <target>me</target>
      <menus></menus>
    </Feast>
  </Stuff>
  val feastWithMenus = <Stuff>
    <Feast>
      <target>me</target>
      <menus>
        <item>
          <first>pate</first>
          <mains>beef</mains>
          <dessert>ice-cream</dessert>
        </item>
        <item>
          <first>soup</first>
          <mains>omlette</mains>
          <dessert>ice-cream</dessert>
        </item>
      </menus>
    </Feast>
  </Stuff>

  def justParse() {
    parse(singleWrapped) match {
      case dinner: DinnerIn =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menu.dessert == "ice-cream", s"not icecreaem ${dinner.menu.dessert}")
      case x => assert(false, s"oh dear got $x")
    }

    parse(optionWrappedNoFriend) match {
      case dinner: DinnerInWith =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menu.dessert == "ice-cream", s"not icecreaem ${dinner.menu.dessert}")
      case x => assert(false, s"oh dear got $x")
    }
    parse(optionWrappedFriend) match {
      case dinner: DinnerInWith =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menu.dessert == "ice-cream", s"not icecreaem ${dinner.menu.dessert}")
        assert(dinner.friend == Some("fred"), s"Not fred instead ${dinner.friend}")
      case x => assert(false, s"oh dear got $x")
    }

    parse(feastWithMenus) match {
      case dinner: Feast =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menus.size == 2, s"odd menus ${dinner.menus}")
        assert(dinner.menus.head.first == "pate", s"not pate ${dinner.menus.head}")
      case x => assert(false, s"oh dear got $x")
    }

    // what does it actually look like
    println(feastNoMenu)
    parse(feastNoMenu) match {
      case dinner: Feast =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menus == List.empty, s"odd menus ${dinner.menus}")
      case x => assert(false, s"oh dear got $x")
    }
  }

  def write(msg: Msg): NodeSeq = {

    val xmlWriter = XmlWriter[Msg]
    <Stuff>{xmlWriter.write(None, msg)}</Stuff>
  }

  def inOut(xml: NodeSeq): Msg = {
    val first = parse(xml)
    val written = write(first)
    println(s"---------------------------\n$written\n${written}")
    def second = parse(written)
    assert(first == second, s"mismatch \n${first} and \n${second}\n${written}")
    second
  }

  justParse()

  val m1 = Menu("soup", "beef", "ice-cream")
  val dinnerIn = DinnerIn("me", m1)
  val dinnerInWritten = write(dinnerIn)

  println(s"dinnerInWritten is $dinnerInWritten\n${dinnerInWritten}")


  inOut(feastNoMenu)
  inOut(feastWithMenus)

  inOut(optionWrappedFriend)
  inOut(optionWrappedNoFriend)

  println("success")
}
