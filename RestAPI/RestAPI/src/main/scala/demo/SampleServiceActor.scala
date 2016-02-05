package demo

import java.io.{FileInputStream, File}
import java.security._
import java.security.spec.{X509EncodedKeySpec, EncodedKeySpec}
import java.util
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, SecretKey}
import spray.http.MediaTypes
import spray.routing.{SimpleRoutingApp, HttpService}
import spray.routing.SimpleRoutingApp
import spray.routing._
import akka.pattern._
import akka.pattern.AskTimeoutException
import spray.httpx.marshalling.ToResponseMarshallable.isMarshallable
import spray.routing.Directive.pimpApply
import spray.routing.directives.ParamDefMagnet.apply
import akka.actor._
import sun.misc.{BASE64Decoder, BASE64Encoder}
import scala.concurrent.duration._
import akka.util._
import Stuff._
import spray.json._
import scala.concurrent.Await
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed trait messages
case class getdata(k:Int) extends messages
case class GetProfile(id:Int) extends messages
case class addPost(id:Int,UPost:String) extends messages
case class getPostlist(id:Int) extends messages
case class getfrndlist(id:Int) extends messages
case class getdetails(id:Int) extends messages
case class addFrndPost(userId:Int,frndId:Int,msg:String) extends messages
case class Start(i:Int) extends  messages
case class AddFriend(index1:Int,index2:Int) extends messages

class Userdata extends messages {
  var Id: Int = 1
  var name: String = "ab"
  var DOB: String="28051990"
  var location:String="Bikaner"
  var Posts=new ArrayBuffer[String]()
  var frndlist= new java.util.HashMap[Int, Userdata]()
  var ProfilePic:String=""
  var albumPic=new ArrayBuffer[String]()
  var frndlistString:String=""
  var postList:String=""
  var frndRequests:ArrayBuffer[Int]= new ArrayBuffer[Int]()
  var mypublicKey:PublicKey=null
  var token:String=null
  var settings:Int= 0
  def UserData() {
  }
}


object SampleServiceActor extends App with SimpleRoutingApp {
  implicit val system = ActorSystem("RESTService");
  var usrdata = new Userdata();
  var usrlist = new util.ArrayList[Userdata]();
  var serverActors = new ArrayBuffer[ActorSelection]()
  val userlist = new java.util.HashMap[Int, Userdata]()

  import system.dispatcher
  import Stuff._

  implicit val timeout = Timeout(3.seconds)

  def bytesToString(bytes: Array[Byte]): String = {
    return new BASE64Encoder().encode(bytes);
  }

  def stringToBytes(string: String): Array[Byte] = {
    return new BASE64Decoder().decodeBuffer(string);
  }

  def generateToken(userId: Int): String = {
    val secureRandom: SecureRandom = new SecureRandom()
    var newSeed: Array[Byte] = secureRandom.generateSeed(10)
    secureRandom.setSeed(newSeed);
    var sessionByte: Array[Byte] = new Array[Byte](10);
    secureRandom.nextBytes(sessionByte);
    val sessionId = new BASE64Encoder()
      .encode(sessionByte)
      .replace("\n", "")
      .replace("\r", "")
      .replace("=", "")
    //  tokenList.put(userId,sessionId)
    sessionId
  }

  def decrypttoken(encodedKey: Array[Byte], mypair: PublicKey,userId:Int): Boolean = {
    var myVerifySign:Signature  = Signature.getInstance("SHA1withRSA");
    myVerifySign.initVerify(userlist.get(userId).mypublicKey);
    myVerifySign.update(stringToBytes(userlist.get(userId).token));
    var verifySign = myVerifySign.verify(encodedKey);
    println("VERUIFY TOKEN-->"+verifySign)
    return verifySign;
  }

  startServer(interface = "localhost", port = 8080) {
    path("intialize" / "mine") {
      post {
        parameter("userId".as[Int] ?, "myPublicKey".as[String],"setng".as[Int]) { (userId, myPublicKey,setng) =>
          println("Initialize the users of facebook")
          var myPub = myPublicKey
          myPub = myPub.replace("_", "\u000d")
          myPub = myPub.replace("~", "\u000a")
          myPub = myPub.replace(" ", "+")
          var keyBytes = stringToBytes(myPub)
          println(userId.getOrElse(0) + "::::" + myPub + "--> mystr---->" + keyBytes)
          var publicKeySpec: EncodedKeySpec = new X509EncodedKeySpec(keyBytes);
          var publicKey2: PublicKey = KeyFactory.getInstance("RSA").generatePublic(publicKeySpec);
          println(userId + " PUBLIC KEY----->" + publicKey2)
          println(userId + "Initialize Time Start: " + System.currentTimeMillis())
          var usrdata = new Userdata()
          val sessionId = generateToken(userId.getOrElse(0))
          println(sessionId)
          usrdata.Id = userId.getOrElse(0)
          usrdata.name = "user" + userId.getOrElse(0)
          usrdata.mypublicKey = publicKey2
          usrdata.token = sessionId
          usrdata.settings=setng
          // usrdata.Posts += "I am added to facebook"
          var t: Int = (userId.getOrElse(0) % 4) + 1
          userlist.put(userId.getOrElse(0), usrdata)
          println("User ADDED TO FACEBOOK" + usrdata.name)
          println(userId.getOrElse(0) + "Initialize Time End: " + System.currentTimeMillis())
          complete(secureToken(sessionId))
        }
      }
    }~
      path(IntNumber / "friendRequest" / IntNumber) { (index1, index2) =>
        get {
          cookie("sessionCookie") { sessionCookie =>
            respondWithMediaType(MediaTypes.`application/json`) {
              var user1key = userlist.get(index1).mypublicKey
              var bytetoken = stringToBytes(sessionCookie.content)
              var mytoken = decrypttoken(bytetoken, user1key, index1)
              if (mytoken == true) {
                println(index1 + "friendRequest Time Start: " + System.currentTimeMillis())
                var user = userlist.get(index2)
                user.frndRequests += index1
                userlist.replace(index2, user)
                println(index1 + "friendRequest Time End: " + System.currentTimeMillis())
                complete {
                  "Friend Request Raised"
                }
              }
              else complete {
                "NOT ALLOWED"
              }
            }
          }
        }
      } ~
      path("AcceptRequest" / IntNumber) { index =>
        get {
          cookie("sessionCookie") { sessionCookie =>
            respondWithMediaType(MediaTypes.`application/json`) {
              var user1key = userlist.get(index).mypublicKey
              var usr = new Userdata()

              var bytetoken = stringToBytes(sessionCookie.content)
              var mytoken = decrypttoken(bytetoken, user1key,index)
              if (mytoken == true) {
                println(index + "AcceptRequest Time Start: " + System.currentTimeMillis())
                usr = userlist.get(index)
                var i = 0
                var requestlist = usr.frndRequests.size
                while (requestlist > 0) {
                  var usrfrnd = userlist.get(usr.frndRequests(i))
                  usr.frndlist.put(usr.frndRequests(i), usrfrnd)
                  usrfrnd.frndlist.put(index, usr)
                  userlist.replace(index, usr)
                  userlist.replace(usr.frndRequests(i), usrfrnd)
                  i += 1
                  requestlist -= 1
                }

                println(index + "AcceptRequest Time End: " + System.currentTimeMillis())
                complete {
                  friendList(usr.frndRequests.toSeq)
                }
              }
              else {complete{"{ Not Authorized }"}}
            }
          }
        }
      }~
      path(IntNumber / "getprofile" / IntNumber) { (index,index1) =>
        get {
          cookie("sessionCookie") { sessionCookie =>
            respondWithMediaType(MediaTypes.`application/json`) {
              var user1key = userlist.get(index).mypublicKey
              var usr = new Userdata()
              //----------------------------------------------------------------------
              var bytetoken = stringToBytes(sessionCookie.content)

              var mytoken = decrypttoken(bytetoken, user1key,index)
              if (mytoken == true) {
                if (userlist.get(index1).settings == 1 || userlist.get(index1).settings == 0) {
                  var set: Int = 0
                  var b: ArrayBuffer[Int] = new ArrayBuffer[Int]()
                  var usrdata = userlist.get(index1)
                  if (usrdata == null) {
                    complete("NO USER")
                  }
                  else {
                    println("USER PROFILE")
                    var frndIdlist = usrdata.frndlist.keySet()
                    var frndarray: Array[AnyRef] = frndIdlist.toArray()
                    var frndbuffer: ArrayBuffer[Int] = new ArrayBuffer[Int]()
                    var itr: util.Iterator[Int] = frndIdlist.iterator()
                    while (itr.hasNext()) {
                      frndbuffer += itr.next()
                    }
                    complete(userInformation(usrdata.Id, usrdata.name, usrdata.DOB, usrdata.location, frndbuffer.toSeq, usrdata.Posts.toSeq, usrdata.ProfilePic))
                  }
                }
                else
                  complete("{ Not Authorized }")
              }
              else complete("{ Not Authorized }")
            }
          }
        }
      } ~
      path("getAlbum" / IntNumber) { index =>
        get {
          cookie("sessionCookie") { sessionCookie =>
            respondWithMediaType(MediaTypes.`application/json`) {
              println("Album")
              var user1key = userlist.get(index).mypublicKey
              var bytetoken = stringToBytes(sessionCookie.content)
              var mytoken = decrypttoken(bytetoken, user1key, index)
              if (mytoken == true) {
                val usrdata=userlist.get(index)
                if (usrdata == null) {
                  complete("NO USER")
                }
                else {
                  if(usrdata.albumPic == null)
                    complete("No album")
                  else {
                    complete(album(usrdata.albumPic.toSeq))
                  }
                }
              }
              else
                complete {
                  "{ Not Authorized }"
                }
            }
          }
        }
      }~
      path("addPost"/"mine") {
        post {
          parameter("userId".as[Int] ?, "frndId".as[Int], "msg".as[String]) { (userId, frndId, msg) =>
            cookie("sessionCookie") { sessionCookie =>
              respondWithMediaType(MediaTypes.`application/json`) {
                println(userId.getOrElse(0) + "addPost Time Start: " + System.currentTimeMillis())
                println("ADDING POST" + userId.getOrElse(0) + "friend Id" + frndId + "MSG-->" + msg)
                var set: Int = 0
                var b = new java.util.HashMap[Int, Userdata]()
                var user1key = userlist.get(userId.getOrElse(0)).mypublicKey
                var bytetoken = stringToBytes(sessionCookie.content)
                var mytoken = decrypttoken(bytetoken, user1key, userId.getOrElse(0))
                if (mytoken == true) {
                  var usrdata = userlist.get(userId.getOrElse(0))
                  if (usrdata != null) {
                    b = usrdata.frndlist
                    if (b.get(frndId) != null) {
                      var frnddata = userlist.get(frndId)
                      frnddata.Posts += (msg)
                      userlist.replace(frndId, frnddata)
                      println("FRIEND PROFILE", userlist.get(frndId).Posts.toString)
                      set = 1
                    }
                    else {
                      var usr2 = userlist.get(frndId)
                      usr2.Posts += (msg)
                      userlist.replace(frndId, usr2)
                      println("FRIEND PROFILE 2", userlist.get(frndId).Posts.toString)
                      set = 1
                    }

                  }
                }
                println(userId + "addPost Time End: " + System.currentTimeMillis())
                if (set == 1) {
                  complete {
                    "ADDED"
                  }
                }
                else if (set == 2) {
                  complete {
                    "RANDOM ADDED"
                  }
                }
                else {
                  complete {
                    "CANNOT ADD"
                  }
                }
              }
            }
          }
        }
      }~
      get {
        path("getPosts" / IntNumber) { index =>
          cookie("sessionCookie") { sessionCookie =>
            respondWithMediaType(MediaTypes.`application/json`) {
              var user1key = userlist.get(index).mypublicKey
              var bytetoken = stringToBytes(sessionCookie.content)
              var mytoken = decrypttoken(bytetoken, user1key, index)
              if (mytoken == true) {
                println(index + "getPost Time Start: " + System.currentTimeMillis())
                var set: Int = 0
                var b: ArrayBuffer[String] = new ArrayBuffer[String]()
                var usrdata = userlist.get(index)
                if (usrdata != null) {
                  b = usrdata.Posts
                  set = 1
                }
                println(index + "getPost Time End: " + System.currentTimeMillis())
                if (set == 1)
                  complete(postList(usrdata.Posts.toSeq))
                else
                  complete("ERROR")
              }
              else
                complete("{ Not Authorized }")
            }
          }
        }
      }~
      get {
        path("getfriends" / IntNumber) { index =>
          cookie("sessionCookie") { sessionCookie =>
            respondWithMediaType(MediaTypes.`application/json`) {
              var user1key = userlist.get(index).mypublicKey
              var bytetoken = stringToBytes(sessionCookie.content)
              var mytoken = decrypttoken(bytetoken, user1key, index)
              if (mytoken == true) {
                println("Getting FRIENDLIST")
                println(index + "getFriends Time Start: " + System.currentTimeMillis())
                var set: Int = 0
                var b = new java.util.HashMap[Int, Userdata]()
                var usrdata = userlist.get(index)
                var itr: util.Iterator[Int] = b.keySet().iterator()
                var frndlist: ArrayBuffer[Int] = new ArrayBuffer[Int]()
                if (usrdata != null) {
                  b = usrdata.frndlist
                  while (itr.hasNext()) {
                    frndlist += itr.next()
                  }
                  set = 1
                }
                println(index + "getFriends Time End: " + System.currentTimeMillis())
                if (set == 1)
                  complete(friendList(frndlist.toSeq))
                else
                  complete("ERROR")

              }
              else complete("{ Not Authorized }")
            }
          }
        }
      }~
      path("postAlbum") {
        post {
          parameter("userId".as[Int] ?, "image".as[String]) { (userId, image) =>
            cookie("sessionCookie") { sessionCookie =>
              respondWithMediaType(MediaTypes.`application/json`) {
                var user1key = userlist.get(userId.getOrElse(0)).mypublicKey
                var bytetoken = stringToBytes(sessionCookie.content)
                var mytoken = decrypttoken(bytetoken, user1key, userId.getOrElse(0))
                if (mytoken == true) {
                  var usrdata = userlist.get(userId.getOrElse(0))
                  if(usrdata == null){
                    complete("No User")
                  }
                  else {
                    var image1 = image
                    image1 =
                      image1.replace("`","=")
                        .replace(" ","+")
                    usrdata.albumPic += image1
                    var t = userId.getOrElse(0)
                    userlist.replace(t,usrdata)
                    complete("Pic Added to Album")
                  }
                }
                else
                  complete("No user session available")
              }
            }
          }
        }
      }~
      path("postProfilePic") {
        post {
          parameter("userId".as[Int] ?, "image".as[String]) { (userId, image) =>
            cookie("sessionCookie") { sessionCookie =>
              respondWithMediaType(MediaTypes.`application/json`) {
                var user1key = userlist.get(userId.getOrElse(0)).mypublicKey
                var bytetoken = stringToBytes(sessionCookie.content)
                var mytoken = decrypttoken(bytetoken, user1key, userId.getOrElse(0))
                if (mytoken == true) {
                  var usrdata = userlist.get(userId.getOrElse(0))
                  if(usrdata == null){
                    complete("No User")
                  }
                  else {
                    var image1 = image
                    image1 =
                      image1.replace("`","=")
                        .replace(" ","+")
                    usrdata.ProfilePic = image1
                    var t = userId.getOrElse(0)
                    userlist.replace(t,usrdata)
                    complete("Profile Pic Added")
                  }
                }
                else
                  complete("No user session available")
              }
            }
          }
        }
      }~
      get {
        complete("I exist!")
      }
  }
}