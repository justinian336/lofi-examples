package responses

import models.{DataPoint, DataPointWithDistance}

case class KNNResponse(point: DataPoint, neighborhood: List[DataPointWithDistance], k: Int)
