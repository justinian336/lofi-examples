package datasources

import generic.GenericUtils
import lof.LOFDataSource
import models.DataPoint
import shapeless.{::, Generic, HList, HNil}
import shapeless.ops.hlist.{LeftFolder, Mapper, Zip}
import tree.{KDTree, Leaf, Node}
import scala.{:: => :::}

import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal

case class LOFKdTree(data: KDTree[DataPoint])(implicit ec: ExecutionContext) extends LOFDataSource[DataPoint]{

  implicit val lofDataSource: LOFKdTree = this

  override def getKNN[H <: HList, K <: HList, L <: HList](p: DataPoint, k: Int)
                                                         (implicit ec: ExecutionContext,
                                                          gen: Generic.Aux[DataPoint, H],
                                                          zipper: Zip.Aux[H :: H :: HNil, L],
                                                          diffMapper: Mapper.Aux[GenericUtils.absDiffMap.type, L, H],
                                                          folder: LeftFolder.Aux[H, BigDecimal, GenericUtils.reducerPoly.type, BigDecimal]): Future[(DataPoint, List[(DataPoint, BigDecimal)], Int)] = {
    Future.successful(data.nnSearch(k, p)).map{n=> (p, n, k)}
  }

  override def getNPLOF[H <: HList, K <: HList, L <: HList](k: Int, p: DataPoint, λ: BigDecimal)
                                                           (implicit ec: ExecutionContext,
                                                            gen: Generic.Aux[DataPoint, H],
                                                            zipper: Zip.Aux[H :: H :: HNil, L],
                                                            diffMapper: Mapper.Aux[GenericUtils.absDiffMap.type, L, H],
                                                            folder: LeftFolder.Aux[H, BigDecimal, GenericUtils.reducerPoly.type, BigDecimal]): Future[BigDecimal] = {

    foldNPLof(BigDecimal(0), List(data), k, λ)

  }


  private def foldNPLof[H <: HList, K <: HList, L <: HList]
  (cumSum : BigDecimal, data: List[KDTree[DataPoint]], k: Int, λ: BigDecimal)
                                (implicit ec: ExecutionContext,
                                 gen: Generic.Aux[DataPoint, H],
                                 zipper: Zip.Aux[H :: H :: HNil, L],
                                 diffMapper: Mapper.Aux[GenericUtils.absDiffMap.type, L, H],
                                 folder: LeftFolder.Aux[H, BigDecimal, GenericUtils.reducerPoly.type, BigDecimal]): Future[BigDecimal] = {
    data match{
      case Nil => Future.successful(cumSum)
      case Leaf(p, _, _):::ls => p.getPLOF(k, λ).flatMap{c=> foldNPLof(cumSum + c, ls, k, λ)}
      case Node(_, _, _, _, left, right):::ls =>
        foldNPLof(cumSum, List(left, right):::ls, k, λ)
    }
  }
}

object LOFKdTree{

  def grow(data: List[DataPoint])(implicit ex: ExecutionContext): Future[LOFKdTree] = {

    KDTree.grow(data).map(LOFKdTree(_))

  }

}
