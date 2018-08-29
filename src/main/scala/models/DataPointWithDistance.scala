package models

import java.util.UUID

import distance.Identifiable

case class DataPointWithDistance(x: BigDecimal, y: BigDecimal, distance: BigDecimal) extends Identifiable{

    def asDataPoint() = DataPoint(x, y)

}
