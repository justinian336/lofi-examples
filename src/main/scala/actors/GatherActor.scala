package actors

import actors.GatherActor.Neighborhood
import actors.ManagerActor.ReturnKNN
import actors.Node.GetLocalKNN
import akka.actor.{Actor, ActorRef, Props}
import models.DataPoint

import scala.collection.mutable.ListBuffer

object GatherActor{
  def props() = Props(GatherActor())

  type Neighborhood = Map[DataPoint, BigDecimal]

  final case class NodesList(nodesList: Set[ActorRef])
  final case class Aggregate(k: Int, dataPoint: DataPoint, neighborhood: Neighborhood, node: ActorRef)
  final case class GetGlobalKNN(k: Int, dataPoint: DataPoint)
  final case class nPLOF(λ: BigDecimal)

}

case class BufferEntry(dataPoint: DataPoint, respondingActors: Map[ActorRef, Neighborhood])

case class GatherActor() extends Actor {
  import actors.GatherActor._

  val buffer = new ListBuffer[BufferEntry]

  var nodes: Set[ActorRef] = Set()

  override def receive: Receive = {

    case nodesList: Set[ActorRef] =>
      nodes = nodesList


    case GetGlobalKNN(k, dataPoint) =>
      val currentSender = sender

      nodes.par.foreach{_ tell(GetLocalKNN(k, dataPoint), currentSender)}

    case Aggregate(k, dataPoint, neighborhood, node) =>
      val buffered: Option[BufferEntry] =
        buffer
        .find(_.dataPoint.id == dataPoint.id)

      buffered match{
        case Some(bufferEntry)=>
          if(bufferEntry.respondingActors.size == nodes.size - 1){

//        This is the last message, obtain the global neighborhood
            val globalNeighborhood: List[(DataPoint, BigDecimal)]=
              (bufferEntry
                .respondingActors
                .flatMap{case(_, localNeighborhood)=> localNeighborhood} ++ neighborhood)
                .toList
                .sortBy{case (_, dist)=>dist}
                .take(k)

//          Remove the entry from the buffer
            val index = buffer.indexWhere(_.dataPoint.id == bufferEntry.dataPoint.id)
            buffer.remove(index)

            val currentSender = sender
            context.parent tell(ReturnKNN(k, bufferEntry.dataPoint, globalNeighborhood), currentSender)
          } else {
            val updatedEntry = bufferEntry.copy(respondingActors = bufferEntry.respondingActors ++ Set(node -> neighborhood))

            val index = buffer.indexWhere(_.dataPoint.id == bufferEntry.dataPoint.id)
            buffer.update(index, updatedEntry)
          }

//        If this is the first message, create a new entry
        case None =>
          buffer.append(BufferEntry(dataPoint, Map(sender -> neighborhood)))
      }
  }
}
