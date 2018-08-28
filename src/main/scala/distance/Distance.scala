package distance

import lof.LOFDataPoint

trait Distance[T<:LOFDataPoint[T]] {

  def distance(other: T): BigDecimal

}
