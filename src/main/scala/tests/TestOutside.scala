package tests

import scala.xml.NodeSeq
// import shapeless._
import shapeless.labelled._

/**
 * Created by tim on 15/01/15.
 */
class TestOutside {
  object TestXmlSimple extends App {

    import Common._
    import XmlSupport2._

    implicit val NoMsgParser = XmlParser[NoMsg.type]
    implicit val NoMsgWriter = XmlWriter[NoMsg.type]
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

}
