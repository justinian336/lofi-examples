package models

import java.util.UUID

import distance.DistanceAlgos
import lof.LOFDataPoint

object DataPoint{

  def withRandomId(x: BigDecimal, y: BigDecimal) = DataPoint(UUID.randomUUID(), x, y)

  def random(xRange: Range, yRange: Range) = {
    val x = (math.random()*(xRange.end - xRange.start) + xRange.start).toInt
    val y = (math.random()*(yRange.end - yRange.start) + yRange.start).toInt
    DataPoint.withRandomId(x, y)
  }
}

case class DataPoint(id: UUID, x: BigDecimal, y: BigDecimal) extends LOFDataPoint[DataPoint] {
  override def distance(other: DataPoint): BigDecimal = DistanceAlgos.euclidean(this, other)

  override def toString: String = s"($x, $y)"

  def appendDistance(distance: BigDecimal): DataPointWithDistance = DataPointWithDistance(id, x, y, distance)
}
