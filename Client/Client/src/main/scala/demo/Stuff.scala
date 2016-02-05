package demo

import spray.httpx.SprayJsonSupport
import spray.json._

case class picture(profilePic: String)

object Stuff extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val pictureFormat = jsonFormat1(picture.apply)
}