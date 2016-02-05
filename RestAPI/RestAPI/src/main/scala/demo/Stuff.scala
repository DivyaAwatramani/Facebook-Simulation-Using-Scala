package demo

import spray.httpx.SprayJsonSupport
import spray.json._

case class userDetails(id: Int, name: String, dob: String, location: String)
case class picture(profilePic: String)
case class postList(post: Seq[String])
case class album(pic: Seq[String])
case class friendList(friends: Seq[Int])
case class secureToken(token: String)
case class userInformation(id: Int, name: String, dob: String, location: String,  friends: Seq[Int], post: Seq[String], profilePic: String)

object Stuff extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val userDetailsFormat = jsonFormat4(userDetails.apply)
  implicit val pictureFormat = jsonFormat1(picture.apply)
  implicit val secureTokenFormat = jsonFormat1(secureToken.apply)

  implicit object postListFormat extends RootJsonFormat[postList] {
    override def write(obj: postList): JsValue = JsObject(Map(
      "post" -> JsArray(obj.post.map(_.toJson).toJson)
    ))
    override def read(json: JsValue): postList = postList(json.convertTo[Seq[String]])
  }

  implicit object friendListFormat extends RootJsonFormat[friendList] {
    override def write(obj : friendList): JsValue = JsObject(Map(
      "friends" -> JsArray(obj.friends.map(_.toJson).toJson)
    ))
    override def read(json: JsValue): friendList = friendList(json.convertTo[Seq[Int]])
  }

  implicit object albumFormat extends RootJsonFormat[album] {
    override def write(obj: album): JsValue = JsObject(Map(
      "pic" -> JsArray(obj.pic.map(_.toJson).toJson)
    ))
    override def read(json: JsValue): album = album(json.convertTo[Seq[String]])
  }

  implicit object userInformationFormat extends RootJsonFormat[userInformation] {
    override def read(json: JsValue): userInformation = json match {
      case JsArray(Vector(JsNumber(id), JsString(name), JsString(dob), JsString(location), JsArray(Vector(friends)), JsArray(Vector(post)), JsString(profilePic))) =>
        new userInformation(id.toInt, name, dob, location, friends.convertTo[Seq[Int]], post.convertTo[Seq[String]], profilePic)
      case _ => deserializationError("UserInformation expected")
    }
    override def write(usrInfo: userInformation):JsValue = JsObject(Map(
      "id" -> JsNumber(usrInfo.id),
      "name" -> JsString(usrInfo.name),
      "dob" -> JsString(usrInfo.dob),
      "location" -> JsString(usrInfo.location),
      "friends" -> JsArray(usrInfo.friends.map(_.toJson).toJson),
      "post" -> JsArray(usrInfo.post.map(_.toJson).toJson),
      "profilePic" -> JsString(usrInfo.profilePic)
    ))
  }

}