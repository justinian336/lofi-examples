package models

import distance.Point.rnd
import lof.LOFDataPoint

object DataPoint{

  def withRandomId(x: BigDecimal, y: BigDecimal) = DataPoint(x, y)

  def getRandomBigDecimal(range: Range) =
    BigDecimal(rnd.nextDouble()*(range.end - range.start) + range.start)

  def random(xRange: Range, yRange: Range) = {
    val x = getRandomBigDecimal(xRange)
    val y = getRandomBigDecimal(yRange)
    DataPoint.withRandomId(x, y)
  }
}

case class DataPoint(x: BigDecimal, y: BigDecimal) extends LOFDataPoint[DataPoint] {

  override def toString: String = s"($x, $y)"

  def appendDistance(distance: BigDecimal): DataPointWithDistance = DataPointWithDistance(x, y, distance)
}
