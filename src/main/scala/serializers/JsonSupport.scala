package serializers

import java.util.UUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import models.{DataPoint, DataPointWithDistance}
import responses.KNNResponse
import spray.json._

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol{

  implicit val uuidFormat = new JsonFormat[UUID]{
    override def write(obj: UUID): JsValue = JsString(obj.toString)
    override def read(json: JsValue): UUID = {
      json match{
        case JsString(value) => UUID.fromString(value)
        case _ => throw new Exception("Failed to unmarshal UUID")
      }
    }
  }

  implicit val dataPointFormat = jsonFormat(DataPoint.apply, "x", "y")
  implicit val dataPointWithDistanceFormat = jsonFormat(DataPointWithDistance.apply, "x", "y", "distance")
  implicit val knnFormat = jsonFormat3(KNNResponse.apply)
}
