package actors

import actors.GatherActor.Aggregate
import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import models.DataPoint

import scala.concurrent.ExecutionContext

object Node {

  def props(gatherActor: ActorRef)(implicit timeout: Timeout, ec: ExecutionContext): Props = Props(Node(gatherActor))

  final case class Append(data: List[DataPoint])
  final case class GetLocalKNN(k: Int, point: DataPoint)
  final case class GetNPLOF(k: Int, point: DataPoint)
  final case class GetData()

}

case class Node(gatherActor: ActorRef)(implicit val timeout: Timeout, ec: ExecutionContext) extends Actor{
  import Node._

  var data: List[DataPoint] = Nil

  override def receive: Receive = {

    case Append(newData) =>
      data = data ++ newData
      val currentSender = sender
      currentSender ! Unit

    case GetLocalKNN(k, p) =>
      val neighborhood = data.map{neighbor=>
        neighbor -> neighbor.distance(p)
      }.sortBy{case (_, dist) => dist}.take(k).toMap

      val currentSender = sender
      gatherActor tell(Aggregate(k, p, neighborhood, self), currentSender)

  }
}
