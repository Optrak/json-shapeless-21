package tests

/**
 * Created by tim on 30/12/14.
 */
object Common {
  case class Menu(first: String, mains: String, dessert: String)

  sealed trait Msg 
  
  case class Invite(target: String, place: String, nGuests: Int) extends Msg

  case class DinnerInWith(target: String, menu: Menu, friend: Option[String]) extends Msg

  case class DinnerOut(target: String, menu: Option[Menu]) extends Msg

  case class Feast(target: String, menus: List[Menu]) extends Msg

  case class DinnerIn(target: String, menu: Menu) extends Msg
  
  case object NoMsg extends Msg
  

}
