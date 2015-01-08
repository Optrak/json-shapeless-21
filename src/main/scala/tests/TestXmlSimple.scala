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

object XmlParser extends LabelledTypeClassCompanion[XmlParser] {
  implicit val typeClass: LabelledTypeClass[XmlParser] = new LabelledTypeClass[XmlParser] {

    def emptyProduct: XmlParser[HNil] =
      new XmlParser[HNil] {
        def parse(s: NodeSeq) = HNil
      }

    def product[F, T <: HList](fieldName: String, scf: XmlParser[F], sct: XmlParser[T]) = 
      new XmlParser[F :: T] {
        def parse(jv: NodeSeq): F :: T = {
          val child = jv \ fieldName
          //val child = childish.headOption.getOrElse(NodeSeq.Empty)
          println(s"parse product from $jv looking for $fieldName found as $child")
          val answer = { 
            val front = scf.parse(child)
            println(s"hcons front is $front")
            val back = sct.parse(jv)
            println(s"hcons back is $back")
            front :: back
          }
          println(s"HCons answer is $answer")
          answer
        }
      }

    implicit def emptyCoproduct: XmlParser[CNil] = new XmlParser[CNil] {
      def parse(s: NodeSeq): CNil = throw new Exception("no coproduct found")
    }

    implicit def coproduct[L, R <: Coproduct](fieldName: String, scl: => XmlParser[L], scr: => XmlParser[R]) =
      new XmlParser[L :+: R] {
        override def parse(jv: NodeSeq): L :+: R = {
          val child = jv \ fieldName
          println(s"parse for deriveCCons with $jv looking for $fieldName found as $child")
          val answer = child match {
            case NodeSeq.Empty =>
              println(s"trying cr.parse for $jv")
              Inr(scr.parse(jv))
              /*try {
                println(s"trying cr.parse for $jv")
                Inr(scr.parse(jv))
              } catch {
                case e: Exception => {
                  Inl(scl.parse(jv))
                }
              }*/
            case nn =>
              println(s"found coproduct $fieldName, now parse it")
              Inl(scl.parse(nn))
          }
          println(s"coProduct answer is $answer")
          answer
        }

      }

    implicit def project[F, G](instance: => XmlParser[G], to: F => G, from: G => F) =
      new XmlParser[F] {
        def parse(s: NodeSeq): F = {
          println(s"project, parsing from $s")
          from(instance.parse(s))
        }
      }
  }
}

trait XmlWriter[A] {
  def write(name: Option[String], a: A): NodeSeq
}

object XmlWriter extends LabelledTypeClassCompanion[XmlWriter] {
  def apply[T](implicit w: XmlWriter[T]) = w

  def toNodeSeq(nameOpt: Option[String], ns: NodeSeq): NodeSeq = 
    nameOpt.map(n => Elem(null, n, Null, TopScope, true, ns: _*)).getOrElse(NodeSeq.Empty)

  def toNodeSeq(nameOpt: Option[String], s: String): NodeSeq = 
    nameOpt.map(n => Elem(null, n, Null, TopScope, true, Text(s.trim))).getOrElse(NodeSeq.Empty)

  def toNodeSeq(nameOpt: Option[String], content: Seq[NodeSeq]): NodeSeq = 
    nameOpt.map(n => Elem(null, n, Null, TopScope, true, content.flatten: _*)).getOrElse(NodeSeq.Empty)
  
  implicit val typeClass = new LabelledTypeClass[XmlWriter] {

    def emptyProduct = new XmlWriter[HNil] {
      def write(name: Option[String], hn: HNil): NodeSeq = {
        println(s"emptyProduct on Write for $name")
        assert(name == None)
        List.empty
      }
    }

    def product[F, T <: HList](fieldName: String, FHead: XmlWriter[F], FTail: XmlWriter[T]) = new XmlWriter[F :: T] {
      def write(name: Option[String], ft: F :: T): NodeSeq = {
        println(s"product write for $fieldName in $ft name = $name")
        val hCons = FHead.write(Some(fieldName), ft.head) ++ FTail.write(None, ft.tail)
        val answer = name.map { _ => 
          toNodeSeq(name, hCons)
        }.getOrElse(hCons)
        println(s"product for $fieldName gives $answer")
        answer
      }
    }

    def emptyCoproduct = new XmlWriter[CNil] {
      def write(name: Option[String], t: CNil) = {
        assert(name == None)
        List.empty
      }
    }

    def coproduct[L, R <: Coproduct] (fieldName: String, CL: => XmlWriter[L], CR: => XmlWriter[R]) = new XmlWriter[L :+: R] {
      override def write(name: Option[String], lr: L :+: R): NodeSeq = {
        println(s"coproduct write for $fieldName name = $name")
        val answer = lr match {
          case Inl(l) => CL.write(Some(fieldName), l) // or List(JField(fieldName, CL.write(None, l)))
          case Inr(r) => CR.write(None, r)
        }
        /*val answer = name.map { name => 
          List(JField(fieldName, a1))
        }.getOrElse(a1)*/
        println(s"answer for coproduct $fieldName is $answer")
        answer
      }
    }

    def project[F, G](instance : => XmlWriter[G], to: F => G, from: G => F) = new XmlWriter[F] {
      def write(name: Option[String], t: F): NodeSeq = {
        val tot = to(t)
        println(s"project for $t name $name to(t) is $tot \nname ${t.getClass.getSimpleName}\n${instance}")
        val answer = instance.write(name, tot)
        println(s"project write $name answer $answer")
        answer
      }
    }
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
    def write(name: Option[String], i: Int)  = toNodeSeq(name, i.toString)
  }
  implicit val StringWriter = new XmlWriter[String] {
    def write(name: Option[String], s: String)  = toNodeSeq(name, s)
  }

  class OptWriter[A](implicit aWriter: XmlWriter[A]) extends XmlWriter[Option[A]] {
    def write(name: Option[String], aOpt: Option[A]) = aOpt.map(a => aWriter.write(name, a)).getOrElse(NodeSeq.Empty)
  }

  class ListWriter[A](label: String)(implicit aWriter: XmlWriter[A]) extends XmlWriter[List[A]] {
    def write(name: Option[String], aList: List[A]): NodeSeq = {
      val items = aList.map(a => toNodeSeq(Some(label), aWriter.write(None, a)))
      println(s"list writer writes $items")
      toNodeSeq(name, items)
    }
  }
  implicit def mkListWriter[A](implicit aWriter: XmlWriter[A]) = new ListWriter[A]("item")
  implicit def mkOptWriter[A](implicit aWriter: XmlWriter[A]) = new OptWriter[A]


}

object TestXmlSimple extends App {

  import Common._
  import XmlSupport2._

  /* Test Data */

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
  
  /* Helpers */
  
  def parse(n: NodeSeq): Msg = {

    val xmlParser = XmlParser[Msg]
    xmlParser.parse(n)
  }

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
  
  /* Tests */

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
