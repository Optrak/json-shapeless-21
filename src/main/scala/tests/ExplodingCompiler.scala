package tests

import scala.xml.NodeSeq

/**
 * Created by tim on 11/01/15.
 *
object ExplodingCompiler {

  import XmlSupport2._

  case class Foo(time: String, i: Int)

  trait SiteBased

 
  case object Admin extends Activity

  case object Break extends Activity

  case object PartialBreak extends Activity

  case object PreDeparture extends Activity with SiteBased

  case object Refuelling extends Activity

  case object AccidentOrBreakdown extends Activity

  case object Briefing extends Activity with SiteBased

  case object Debriefing extends Activity with SiteBased

  case object CheckingVehicle extends Activity

  case object CheckingLoad extends Activity

  case object Waiting extends Activity

  case object DailyRest extends Activity

  case object UnattendedLoading extends Activity with SiteBased

  case object Parking extends Activity with SiteBased

  case object LoadingPreWork extends Activity with SiteBased

  case object LoadingPostWork extends Activity with SiteBased

  case object VehicleIdle extends Activity
*
  case class Loading(
                      // loads: Set[String],
                      // unloads: Set[String],
                      site: String,
                      id: String,
                      isPreOrPostLoading: Int
                      ) extends Activity {
    // override def toString = s"""Loading($id) Loads ${loads.map(_.toString).mkString(", ")} Unloads ${unloads.map(_.toString).mkString(", ")}"""

  }

  object DistanceType extends Enumeration {
    val MatrixDistance = Value("Matrix")
    val ActualDistance = Value("Actual")
    val InterpolatedDistance = Value("Interpolated") // used where we have a break or other non-drive activity between
  }

  implicit val DistanceTypeParser = new XmlParser[DistanceType.Value] {
    def parse(n: NodeSeq) = ???
  }

  //  case class Driving( distance: Int, distanceType: DistanceType.Value = DistanceType.MatrixDistance) extends Activity


  //   case class Complex(foo: Foo)
}
sealed trait Activity
  case class Loading(
                      // loads: Set[String],
                      // unloads: Set[String],
                      site: String,
                      id: String,
                      isPreOrPostLoading: Int
                      ) extends Activity 
object TestIt {
  implicit val IntParser = new XmlParser[Int] {
  def parse(n: NodeSeq) = n.head.text.trim.toInt
}
  implicit val StringParser = new XmlParser[String] {
    def parse(n:NodeSeq) = n.head.text.trim
  }


  implicit val LoadingParser = XmlParser[Loading]
  implicit val StuffParser = XmlParser[Activity]

}
*/