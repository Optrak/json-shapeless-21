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


object JsonParser {
  def apply[T](implicit st: Lazy[JsonParser[T]]): JsonParser[T] = st.value


  implicit def deriveHNil: JsonParser[HNil] =
    new JsonParser[HNil] {
      def parse(s: JValue) = 
        // if (s == JNothing) Some(HNil) else None
        HNil
    }

  implicit def deriveHCons[K <: Symbol, V, T <: HList]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[JsonParser[V]],
   sct: Lazy[JsonParser[T]]
    ): JsonParser[FieldType[K, V] :: T] =
    new JsonParser[FieldType[K, V] :: T] {
      def parse(jv: JValue): FieldType[K, V] :: T = {
        val child = jv \ key.value.name
        println(s"dervieHCons with jv $jv looking for ${key.value.name} found as $child")
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

  implicit def deriveCNil: JsonParser[CNil] = new JsonParser[CNil] {
    def parse(s: JValue): CNil = throw new Exception("no coproduct found")
  }

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[JsonParser[V]],
   sct: Lazy[JsonParser[T]]
    ): JsonParser[FieldType[K, V] :+: T] =
    new JsonParser[FieldType[K, V] :+: T] {
      def parse(jv: JValue): FieldType[K, V] :+: T = {
        val child = (jv \ key.value.name)
        println(s"parse for dervieCCons with $jv looking for ${key.value.name} found as $child")
        val answer = child match {
          case JNothing =>
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
  (implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[JsonParser[G]]): JsonParser[F] =
    new JsonParser[F] {
      def parse(s: JValue): F = {
        val parsed = sg.value.parse(s)
        gen.from(parsed)
      }
    }

}

object JsonBits {
  type JResult = List[JField]
}
import JsonBits._
trait JsonWriter[A] {
  def write(name: Option[String], a: A): JResult
}

object JsonWriter {
  def apply[T](implicit st: Lazy[JsonWriter[T]]): JsonWriter[T] = st.value

  def toJResult(nameOpt: Option[String], ns: JResult): JResult = nameOpt.map(name => List(JField(name, ns))).getOrElse(List.empty)

  def toJResult(nameOpt: Option[String], s: String): JResult = nameOpt.map(name => List(JField(name, s))).getOrElse(List.empty)

  def toJResult(nameOpt: Option[String], content: Seq[JResult]): JResult = nameOpt.map(name => List(JField(name, content))).getOrElse(List.empty)

  implicit def deriveHNil: JsonWriter[HNil] =
    new JsonWriter[HNil] {
      def write(name: Option[String], n: HNil) = List.empty
    }

  implicit def deriveHCons[K <: Symbol, V, T <: HList]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[JsonWriter[V]],
   sct: Lazy[JsonWriter[T]]
    ): JsonWriter[FieldType[K, V] :: T] =
    new JsonWriter[FieldType[K, V] :: T] {
      def write(name: Option[String], ft: FieldType[K, V] :: T): JResult = {
        println(s"hcons writer name $name key ${key.value.name}")
        val h = scv.value.write(Some(key.value.name), ft.head)
        val t = sct.value.write(None, ft.tail)
        println(s"hcons writer name $name key ${key.value.name} h is $h t is $t")
        // this is too much like magic
        val answer = name.map { nm =>
          List(JField(nm, h ::: t))
        }.getOrElse(h ::: t)
        println(s"hcons answer is $answer")
        answer
      }
    }


  implicit def deriveCNil: JsonWriter[CNil] = new JsonWriter[CNil] {
    def write(name: Option[String], t: CNil) = List.empty
  }

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct]
  (implicit
   key: Witness.Aux[K],
   scv: Lazy[JsonWriter[V]],
   sct: Lazy[JsonWriter[T]]
    ): JsonWriter[FieldType[K, V] :+: T] =
    new JsonWriter[FieldType[K, V] :+: T] {
      def write(name: Option[String], lr: FieldType[K, V] :+: T): JResult = {
        println(s"ccons write for ${key.value.name} name $name")
        lr match {
          case Inl(l) => scv.value.write(Some(key.value.name), l)
          case Inr(r) => sct.value.write(Some(key.value.name), r)
        }
      }
    }

  implicit def deriveInstance[F, G]
  (implicit gen: LabelledGeneric.Aux[F, G], sg: Lazy[JsonWriter[G]]): JsonWriter[F] =
    new JsonWriter[F] {
      def write(name: Option[String], t: F): JResult = sg.value.write(name, gen.to(t))
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
    def write(nameOpt: Option[String], i: Int)  = toJResult(nameOpt, i.toString)
  }
  implicit val StringWriter = new JsonWriter[String] {
    def write(nameOpt: Option[String], s: String)  = toJResult(nameOpt, s)
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

  def parse(n: JValue): Msg = {

    val jsonParser = JsonParser[Msg]
    jsonParser.parse(n)
  }
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
    val first= parse(xml)
    val written = write(first)
    println(s"---------------------------\n$written\n${compact(render(written))}")
    def second = parse(written)
    assert(first == second, s"mismatch \n${first} and \n${second}\n${written}")
    second
  }
  
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