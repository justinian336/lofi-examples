package datasources

import lof.LOFDataSource
import models.DataPoint

import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal
import generic.GenericUtils.{absDiffMap, reducerPoly}
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.{LeftFolder, Mapper, Zip}

case class LOFList(data: List[DataPoint]) extends LOFDataSource[DataPoint]{

  implicit val lofDataSource: LOFList = this

  override def getKNN[H<: HList, K<:HList, L<: HList]
  (p: DataPoint, k: Int)
  (implicit ec: ExecutionContext,
   gen: Generic.Aux[DataPoint, H],
   zipper: Zip.Aux[H::H::HNil, L],
   diffMapper: Mapper.Aux[absDiffMap.type, L, H],
   folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]): Future[(DataPoint, List[(DataPoint, BigDecimal)], Int)] = {
    val neighborhood = data.par.map{neighbor=>
      neighbor -> neighbor.distance(p)
    }.toList.sortBy{case (_, dist) =>
      dist
    }.take(k)

    Future.successful {
      (p, neighborhood, k)
    }
  }

  override def getNPLOF[H<: HList, K<:HList, L<: HList]
  (k: Int, p: DataPoint, λ: BigDecimal)
  (implicit ec: ExecutionContext,
   gen: Generic.Aux[DataPoint, H],
   zipper: Zip.Aux[H::H::HNil, L],
   diffMapper: Mapper.Aux[absDiffMap.type, L, H],
   folder: LeftFolder.Aux[H, BigDecimal, reducerPoly.type, BigDecimal]): Future[BigDecimal] = {
    Future.sequence(
      data.map(_.getPLOF(k, λ))
    ).map(_.foldLeft(BigDecimal.valueOf(0)){_ + _})
  }
}
