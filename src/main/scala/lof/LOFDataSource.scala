package lof

import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal

trait LOFDataSource[T<:LOFDataPoint[T]] {

  def getKNN(p: T, k: Int): Future[(LOFDataPoint[T], List[(T, BigDecimal)], Int)]

  def getNPLOF(k: Int, p: T, λ: BigDecimal)(implicit ec: ExecutionContext): Future[BigDecimal]

}
