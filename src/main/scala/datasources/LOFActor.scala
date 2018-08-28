package datasources

import actors.ManagerActor.{GetKNN, Initialize}
import akka.actor.ActorRef
import akka.http.javadsl.ServerBinding
import akka.http.javadsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import lof.LOFDataSource
import models.DataPoint

import scala.concurrent.{ExecutionContext, Future}

case class LOFActor(managerActor: ActorRef)(implicit timeout: Timeout) extends LOFDataSource[DataPoint]{

  override def getKNN(p: DataPoint, k: Int): Future[(DataPoint, List[(DataPoint, BigDecimal)], Int)] = {
        (managerActor ? GetKNN(k, p))
          .mapTo[(DataPoint, List[(DataPoint, BigDecimal)], Int)]
  }

  override def getNPLOF(k: Int, p: DataPoint, λ: BigDecimal)(implicit ec: ExecutionContext): Future[BigDecimal] = ???

  def initialize(data: List[DataPoint], nodes: Int, route: Route)(done: (Route) => ServerBinding)(implicit ec: ExecutionContext): Future[ServerBinding] = {
    (managerActor ? Initialize(data, nodes)).map{ _=>
      done(route)
    }
  }
}
