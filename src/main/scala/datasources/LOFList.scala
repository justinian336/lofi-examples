package datasources

import lof.LOFDataSource
import models.DataPoint

import scala.concurrent.{ExecutionContext, Future}

case class LOFList(data: List[DataPoint]) extends LOFDataSource[DataPoint]{

  implicit val lOFDataSource: LOFList = this

  override def getKNN(p: DataPoint, k: Int): Future[(DataPoint, List[(DataPoint, BigDecimal)], Int)] = {
    val neighborhood = data.par.map{neighbor=>
      neighbor -> neighbor.distance(p)
    }.toList.sortBy{case (_, dist) =>
      dist
    }.take(k)

    Future.successful {
      (p, neighborhood, k)
    }
  }

  override def getNPLOF(k: Int, p: DataPoint, λ: BigDecimal)(implicit ec: ExecutionContext): Future[BigDecimal] = {
    Future.sequence(
      data.map(_.getPLOF(k, λ))
    ).map(_.foldLeft(BigDecimal.valueOf(0)){_ + _})
  }
}
