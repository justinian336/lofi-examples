import actors.ManagerActor
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.util.Timeout
import datasources.{LOFKdTree, LOFList}
import shapeless._

import scala.concurrent.Future
//import datasources.{LOFActor, LOFList}
import models.{DataPoint, DataPointWithDistance}
import serializers.JsonSupport
//import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

object Main extends App with JsonSupport{

    val k = 50
    val n = 20000

//  implicit val lofList: Future[LOFKdTree] = LOFKdTree.grow(dummyData)
  implicit val actorSystem: ActorSystem = ActorSystem("knn-search")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ctx: ExecutionContext = actorSystem.dispatcher
  implicit val timeout: Timeout = Timeout(60 seconds)
  val managerActor = actorSystem.actorOf(ManagerActor.props(), "manager")
//  implicit val lofActor: LOFActor = LOFActor(managerActor)

  val nodes = 4

  println("Creating data...")
  val dataCluster1: List[DataPoint] = (1 to n/2).toList.map{_=>DataPoint.random(Range.inclusive(0, 2000), Range.inclusive(0, 2000))}
  val dataCluster2: List[DataPoint] = (1 to n/2).toList.map{_=>DataPoint.random(Range.inclusive(4000, 10000), Range.inclusive(4000, 5000))}
  val dummyData = dataCluster1 ++ dataCluster2

  println("Indexing data...")

  LOFKdTree.grow(dummyData).map{implicit data=>
    println("The data is ready")
    val route: Route = get{
      path("knn"){
        parameters("x", "y"){(x, y) =>
          val knn = data.getKNN(DataPoint.withRandomId(BigDecimal(x), BigDecimal(y)), k).map{case (p, neighborhood, _)=>
            (p, neighborhood.map{case (q: DataPoint, d)=> DataPointWithDistance(q.x, q.y, d)}, k)
          }
          complete(knn)
        }
      } ~
        path("lof"){
          parameters("x", "y"){(x, y)=>
            val lof = DataPoint.withRandomId(BigDecimal(x), BigDecimal(y)).getLOF(k).map(_.toString)
            complete(lof)
          }
        } ~
        path("lop"){
          parameters("x", "y"){(x, y)=>
            val plof = DataPoint.withRandomId(BigDecimal(x), BigDecimal(y)).getLoOP(k, BigDecimal(1.96)).map(_.toString())
            complete(plof)
          }
        }
    }

    //  (managerActor ? Initialize(dummyData, nodes)).foreach{_=>
    //    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    //  }

    Http().bindAndHandle(route, "localhost", 8080)
  }

}
