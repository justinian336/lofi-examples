package actors

import actors.GatherActor.GetGlobalKNN
import actors.ScatterActor.Scatter
import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import models.DataPoint

import scala.concurrent.ExecutionContext

object ManagerActor{

  def props()(implicit timeout: Timeout, ec: ExecutionContext): Props = Props(ManagerActor())

  final case class Initialize(data: List[DataPoint], nodes: Int)
  final case class GetKNN(k: Int, dataPoint: DataPoint)
  final case class ReturnKNN(k: Int, dataPoint: DataPoint, kNN: List[(DataPoint, BigDecimal)])

}

case class ManagerActor()(implicit val timeout: Timeout, ec: ExecutionContext) extends Actor{
  import ManagerActor._

  val scatterActor: ActorRef = context.actorOf(ScatterActor.props(), "scatter")
  val gatherActor: ActorRef = context.actorOf(GatherActor.props(), "gather")

  def initialize: Receive = {
    case Initialize(dataset, nodes) =>
      (scatterActor ? Scatter(nodes, dataset, gatherActor)).map{_=>
        context.become(kNN)
      } pipeTo sender
  }

  def kNN: Receive = {
    case GetKNN(k, dataPoint) =>
      val currentSender = sender

      gatherActor.tell(GetGlobalKNN(k, dataPoint), currentSender)

    case ReturnKNN(k, dataPoint, kNN) =>
      sender ! (dataPoint, kNN, k)
  }

  override def receive: Receive = initialize
}
