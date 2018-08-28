package distance


import models.DataPoint

import scala.math.{pow, sqrt}

object DistanceAlgos {

  def euclidean(p: DataPoint, q: DataPoint): BigDecimal = {
    sqrt(pow((p.x - q.x).toDouble, 2) + pow((p.y - q.y).toDouble, 2))
  }

}
