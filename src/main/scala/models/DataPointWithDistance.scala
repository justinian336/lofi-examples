package models

import java.util.UUID

case class DataPointWithDistance(id: UUID, x: BigDecimal, y: BigDecimal, distance: BigDecimal) extends Identifiable{

    def asDataPoint() = DataPoint(id, x, y)

}
