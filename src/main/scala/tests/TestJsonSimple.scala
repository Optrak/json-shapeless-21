package tests

/**
 * Created by tim on 28/12/14.
 */

import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import shapeless._
import org.json4s.native.JsonMethods.{compact, render}
import shapeless.labelled._

trait JsonParser[A] {
  def parse(n: JValue): A
}


object JsonParser extends LabelledTypeClassCompanion[JsonParser] {
  implicit val typeClass: LabelledTypeClass[JsonParser] = new LabelledTypeClass[JsonParser] {

    def emptyProduct: JsonParser[HNil] =
      new JsonParser[HNil] {
        def parse(s: JValue) = HNil
      }

    def product[F, T <: HList](fieldName: String, scf: JsonParser[F], sct: JsonParser[T]) = 
      new JsonParser[F :: T] {
        def parse(jv: JValue): F :: T = {
          val child = jv \ fieldName
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

    implicit def emptyCoproduct: JsonParser[CNil] = new JsonParser[CNil] {
      def parse(s: JValue): CNil = throw new Exception("no coproduct found")
    }

    implicit def coproduct[L, R <: Coproduct](fieldName: String, scl: => JsonParser[L], scr: => JsonParser[R]) =
      new JsonParser[L :+: R] {
        override def parse(jv: JValue): L :+: R = {
          val child = jv \ fieldName
          println(s"parse for deriveCCons with $jv looking for $fieldName found as $child")
          val answer = child match {
            case JNothing =>
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

    implicit def project[F, G](instance: => JsonParser[G], to: F => G, from: G => F) =
      new JsonParser[F] {
        def parse(s: JValue): F = {
          println(s"project, parsing from $s")
          from(instance.parse(s))
        }
      }
  }
}

object JsonBits {
  type JResult = List[JField]
  //trait RealCoproduct
}

import JsonBits._

trait JsonWriter[A] {
  def write(name: Option[String], a: A): JResult
}

object JsonWriter extends LabelledTypeClassCompanion[JsonWriter] {
  def apply[T](implicit w: JsonWriter[T]) = w

  def toJResult(nameOpt: Option[String], ns: JValue): JResult = 
    nameOpt.map(name => List(JField(name, ns))).getOrElse(List.empty)

  def toJResult(nameOpt: Option[String], s: String): JResult = 
    nameOpt.map(name => List(JField(name, s))).getOrElse(List.empty)

  def toJResult(nameOpt: Option[String], content: Seq[JValue]): JResult = 
    nameOpt.map(name => List(JField(name, content))).getOrElse(List.empty)
  
  implicit val typeClass = new LabelledTypeClass[JsonWriter] {

    def emptyProduct = new JsonWriter[HNil] {
      def write(name: Option[String], hn: HNil): JResult = {
        println(s"emptyProduct on Write for $name")
        assert(name == None)
        List.empty
      }
    }

    def product[F, T <: HList](fieldName: String, FHead: JsonWriter[F], FTail: JsonWriter[T]) = new JsonWriter[F :: T] {
      def write(name: Option[String], ft: F :: T): JResult = {
        println(s"product write for $fieldName in $ft name = $name")
        val hCons = FHead.write(Some(fieldName), ft.head) ::: FTail.write(None, ft.tail)
        val answer = name.map { name => 
          List(JField(name, hCons))
        }.getOrElse(hCons)
        println(s"product for $fieldName gives $answer")
        answer
      }
    }

    def emptyCoproduct = new JsonWriter[CNil] {
      def write(name: Option[String], t: CNil) = {
        assert(name == None)
        List.empty
      }
    }

    def coproduct[L, R <: Coproduct] (fieldName: String, CL: => JsonWriter[L], CR: => JsonWriter[R]) = new JsonWriter[L :+: R] {
      override def write(name: Option[String], lr: L :+: R): JResult = {
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

    def project[F, G](instance : => JsonWriter[G], to: F => G, from: G => F) = new JsonWriter[F] {
      def write(name: Option[String], t: F): JResult = {
        val tot = to(t)
        println(s"project for $t name $name to(t) is $tot \nname ${t.getClass.getSimpleName}\n${instance}")
        val answer = instance.write(name, tot)
        println(s"project write $name answer $answer")
        answer
      }
    }
  }
  
}


object JsonSupport2 {

  implicit val IntParser = new JsonParser[Int] {
    def parse(n: JValue) = n match {
      case JInt(bi) => bi.toInt
      case JString(str) => str.toInt
      case xx => throw new Exception(s"unexpected thing where should be int $xx")
    }
  }
  implicit val StringParser = new JsonParser[String] {
    def parse(n: JValue) = n match {
      case JString(str) => str
      case xx => throw new Exception(s"unexpected thing where should be STRING $xx")
    }
  }


  class OptParser[A](implicit aParser: JsonParser[A]) extends JsonParser[Option[A]] {
    def parse(n: JValue): Option[A] =
      n match {
        case JNothing => None
        case x => Some(aParser.parse(x))
      }
  }

  class ListParser[A](implicit aParser: JsonParser[A]) extends JsonParser[List[A]] {
    def parse(n: JValue): List[A] = {
      println(s"in ListParser with node $n")
      n match {
        case ja: JArray => ja.arr.map(v => aParser.parse(v))
        case _ => ???
      }
    }
  }
  
  implicit def mkListParser[A](implicit aParser: JsonParser[A]) = new ListParser[A]
  implicit def mkOptParser[A](implicit aParser: JsonParser[A]) = new OptParser[A]

  import JsonWriter._

  implicit val IntWriter = new JsonWriter[Int] {
    def write(nameOpt: Option[String], i: Int) = toJResult(nameOpt, i.toString)
  }
  implicit val StringWriter = new JsonWriter[String] {
    def write(nameOpt: Option[String], s: String) = toJResult(nameOpt, s)
  }

  class OptWriter[A](implicit aWriter: JsonWriter[A]) extends JsonWriter[Option[A]] {
    def write(nameOpt: Option[String], aOpt: Option[A]) = (for {
      name <- nameOpt
      a <- aOpt
    } yield  aWriter.write(nameOpt, a)).getOrElse(List.empty)
  }

  class ListWriter[A](implicit aWriter: JsonWriter[A]) extends JsonWriter[List[A]] {
    def write(name: Option[String], aList: List[A]): JResult = {
      val items = aList.map(a => aWriter.write(None, a))
      println(s"list writer writes $items")
      toJResult(name, items)
    }
  }
  
  implicit def mkListWriter[A](implicit aWriter: JsonWriter[A]) = new ListWriter[A]
  implicit def mkOptWriter[A](implicit aWriter: JsonWriter[A]) = new OptWriter[A]

}

object TestJsonSimple extends App {
  import Common._
  import JsonSupport2._

  /* Test Data */
  
  val singleWrapped = "DinnerIn" ->
    ("target" -> "me") ~
      ("menu" -> (
        ("first" -> "soup") ~
          ("mains" -> "beef") ~
          ("dessert" -> "ice-cream")
        ))

  val optionWrappedNoFriend = "DinnerInWith" -> 
  ("target" -> "me") ~
    ("menu" -> (
      ("first" -> "soup") ~
        ("mains" -> "beef") ~
        ("dessert" -> "ice-cream")
      ))
  

  val optionWrappedFriend = "DinnerInWith" ->
    ("target" -> "me") ~
      ("menu" -> (
        ("first" -> "soup") ~
          ("mains" -> "beef") ~
          ("dessert" -> "ice-cream")
        )) ~
        ("friend"-> "fred")


  val feastWithMenus = "Feast" ->
    ("target" -> "me") ~
      ("menus" -> List(
        ("first" -> "pate") ~
          ("mains" -> "beef") ~
          ("dessert" -> "ice-cream")
        ,
        ("first" -> "soup") ~
          ("mains" -> "omlette") ~
          ("dessert" -> "ice-cream")
      ))

  val feastNoMenu = "Feast" ->
    ("target" -> "me") ~
      ("menus" -> List.empty[JInt])

  
  // this needed for object
  implicit val NoMsgParser = JsonParser[NoMsg.type]
  implicit val NoMsgWriter = JsonWriter[NoMsg.type]

  /* Helpers */
  
      
  def parse(n: JValue): Msg = {
    val jsonParser = JsonParser[Msg]
    jsonParser.parse(n)
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

    println(compact(render(feastWithMenus)))
    parse(feastWithMenus) match {
      case dinner: Feast =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menus.size == 2, s"odd menus ${dinner.menus}")
        assert(dinner.menus.head.first == "pate", s"not pate ${dinner.menus.head}")
      case x => assert(false, s"oh dear got $x")
    }

    // what does it actually look like
    println(compact(render(feastNoMenu)))
    parse(feastNoMenu) match {
      case dinner: Feast =>
        assert(dinner.target == "me", s"not me - ${dinner.target}")
        assert(dinner.menus == List.empty, s"odd menus ${dinner.menus}")
      case x => assert(false, s"oh dear got $x")
    }
  }

  def write(msg: Msg): JValue = {

    val jsonWriter = JsonWriter[Msg]
    jsonWriter.write(None, msg)
  }

  def inOut(xml: JValue): Msg = {
    val first = parse(xml)
    val written = write(first)
    println(s"---------------------------\n$written\n${compact(render(written))}")
    def second = parse(written)
    assert(first == second, s"mismatch \n${first} and \n${second}\n${written}")
    second
  }
  
  /* Tests */
  
  justParse()

  val m1 = Menu("soup", "beef", "ice-cream")
  val dinnerIn = DinnerIn("me", m1)
  val dinnerInWritten = write(dinnerIn)

  println(s"dinnerInWritten is $dinnerInWritten\n${compact(render(dinnerInWritten))}")
  

  inOut(feastNoMenu)
  inOut(feastWithMenus)

  inOut(optionWrappedFriend)
  inOut(optionWrappedNoFriend)

  println("success")
}