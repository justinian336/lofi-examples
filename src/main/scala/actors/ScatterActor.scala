package actors

import actors.Node.Append
import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import models.DataPoint
import akka.pattern.{ask, pipe}

import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode

object ScatterActor {

  def props()(implicit timeout: Timeout, ec: ExecutionContext): Props = Props(ScatterActor())

  final case class Scatter(nodes: Int, dataset: List[DataPoint], gatherActorRef: ActorRef)
}

case class ScatterActor()(implicit val timeout: Timeout, ec: ExecutionContext) extends Actor {
  import actors.ScatterActor._

  private def distribute(n: Int, dataset: List[DataPoint]): List[List[DataPoint]] = {
    dataset.splitAt(n) match {
      case (Nil, Nil) => List(Nil)
      case (a: List[DataPoint], Nil) => a :: Nil
      case (a: List[DataPoint], b: List[DataPoint]) => a :: distribute(n, b)
    }
  }

  override def receive: Receive = {
    case Scatter(nodes, dataset, gatherActorRef) =>
      (1 to nodes).foreach{_=>
        context.actorOf(Node.props(gatherActorRef))
      }

      val children: Set[ActorRef] = context.children.toSet

      gatherActorRef ! children

      val n = (BigDecimal(dataset.size)/ BigDecimal(children.size)).setScale(0, RoundingMode.CEILING).intValue
      val subsets = distribute(n, dataset)
      Future.sequence(
        subsets.zip(context.children).map{case (subData, child)=>
          child ? Append(subData)
        }
      ) pipeTo sender
  }
}
