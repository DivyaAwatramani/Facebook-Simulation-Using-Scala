package demo

import java.io.{FileInputStream, File}
import java.security._
import java.security.KeyFactory
import java.security.spec.{EncodedKeySpec, X509EncodedKeySpec}
import java.util
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{KeyGenerator, Cipher, SecretKey}
import java.security.KeyFactory
import akka.actor.Actor.Receive
import spray.http.HttpHeaders.Cookie
import sun.misc.{BASE64Decoder, BASE64Encoder}
//import akka.actor.Status.Success
import akka.util.Timeout
import spray.http.{HttpEntity, HttpCookie, HttpRequest, HttpResponse}
import scala.util.{ Success, Failure }
import scala.concurrent.{Await, Future}
import scala.util.Random
import java.util.concurrent.TimeUnit
import org.json._
import akka.actor._
import akka.pattern.ask
import scala.concurrent.duration._
import akka.io.IO
import spray.can.Http
import spray.client.pipelining._
import scala.collection.mutable.ArrayBuffer
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import spray.json.DefaultJsonProtocol
import scala.util.{ Success, Failure }
import scala.concurrent.duration.Duration
import Stuff._

sealed trait messages
case class Start (i:Int) extends messages
case class Initialize(i:Int)
case class AddFriend(noOfusr:Int)
case class post(noOfusrs:Int)
case class RaiseFrndRequest(noOfusers:Int)
case class maketable(frnds:ArrayBuffer[Int])
//case class AddKey(id:Int,encryptedAES:Array[Byte])
//case class AddMyKey(id:Int,encryptedAES:Array[Byte])
case class AddKeys(id:Int,encryptedAES:Array[Byte],iV1:IvParameterSpec,iV2:IvParameterSpec)
case class AddMyKeys(id:Int,encryptedAES:Array[Byte],iV1:IvParameterSpec,iV2:IvParameterSpec)
case class SendKey(encryptedAES:Array[Byte],iV1:IvParameterSpec,iV2:IvParameterSpec)
case class SeeProfile(check:Int)
object Main  {
  implicit val system = ActorSystem("facebookClient")
  trait WebClient {
    def get(url: String): Future[String]
  }

  import system.dispatcher

  //If we're on cloud foundry, get's the host/port from the env vars
  lazy val host = Option(System.getenv("VCAP_APP_HOST")).getOrElse("localhost")
  lazy val port = Option(System.getenv("VCAP_APP_PORT")).getOrElse("8080").toInt
  var kpg:   KeyPairGenerator=KeyPairGenerator.getInstance("RSA")
  var kgen:KeyGenerator= KeyGenerator.getInstance("AES")
  var facebookUsers:ArrayBuffer[ActorRef]= new ArrayBuffer[ActorRef]()

  var pipeline = sendReceive
  var clientmaster:ActorRef= null
  // @Override
  def main (args: Array[String]) {
    //println("The number of facebook users"+args(0));
    var noOfUsers:Int=0
    if(args(0)!="")
    noOfUsers=args(0).toInt
    else
    noOfUsers=11

    clientmaster= system.actorOf(Props(new clientMaster(noOfUsers)),"clientmaster")
    clientmaster ! "StartMe"
  }

  class clientMaster(noOfusers:Int) extends  Actor{
    var initCount:Int=0
    var donefrndCount:Int=0
    var donePostCount:Int=0
    var doneAlbumCount:Int=0
    var frndRequestCount:Int=0
    var doneSettingCount:Int=0
    var moreprofile:Int=0
    def receive = {
      case "StartMe"=>
        println("IN START ME")
        var active:Boolean=true
        for(i<-0 until noOfusers) {
          active= !(active)
          var Facebookclient= system.actorOf(Props(new FacebookClient(i,active,noOfusers)),"User"+i)
          facebookUsers+=Facebookclient
        }
        for(i<-0 until noOfusers)
          facebookUsers(i) ! "Start"

      case "doneInit" =>
        initCount+=1
        if(initCount>=(noOfusers).toInt) {

          self ! "RaiseRequest"
        }

      case "RaiseRequest" =>
        for(i<-0 until (noOfusers).toInt)
          facebookUsers(i) ! RaiseFrndRequest(noOfusers)

      case "RaisedRequest" =>
        frndRequestCount+=1
        if(frndRequestCount>=(noOfusers).toInt)
          self ! "acceptfrndRequest"

      case "acceptfrndRequest"=>
        println("ACCPET FRIND REQUEST CALLED")
        for(i<-0 until (noOfusers).toInt)
          facebookUsers(i) ! "acceptRequest"

      case "donefrnd" =>
        println("accept count"+ donefrndCount)
        donefrndCount+=1
        if(donefrndCount==((noOfusers).toInt)-5) {
          println("DONE..................................................!!")
          self ! "AddPost"
        }

      case "startProfile" =>
        for(i<-0 until (noOfusers)){
          facebookUsers(i) ! SeeProfile(1)
          facebookUsers(i) ! "PostAlbum"
        }

      case "AddPost" =>
        for(i<-0 until (noOfusers).toInt)
          facebookUsers(i) ! post(noOfusers)

      case "donePost"=>
        donePostCount+=1
        if(donePostCount>=(noOfusers).toInt){
          println("ADD POST COMPLETE---------------------!!")
          self ! "startProfile"
        }

      case "doneSettings"=>
        doneSettingCount+=1
        if(doneSettingCount>=(noOfusers).toInt) {
          self ! "RaiseRequest"

        }

      case "AlbumPostDone"=>
        doneAlbumCount+=1
        if(doneAlbumCount>=(noOfusers).toInt) {
          self ! "getalbum"
        }

      case "getalbum" =>
        for(i<-0 until (noOfusers).toInt)
          facebookUsers(i) ! "getAlbum"

      case "MoreProfiles" =>
        moreprofile+=1
        if(moreprofile>=noOfusers.toInt) {
          for (i <- 0 until ((noOfusers) / 2).toInt)
            facebookUsers(i) ! SeeProfile(2)
        }
    }
  }

  def generateIV(): IvParameterSpec = {
    var r:SecureRandom  = new SecureRandom()
    var newSeed: Array[Byte]  = r.generateSeed(16)
    r.setSeed(newSeed);
    var byteIV:Array[Byte] = new Array[Byte](16);
    r.nextBytes(byteIV);
    var IV :IvParameterSpec = new IvParameterSpec(byteIV);
    return IV;
  }

  class FacebookClient(id: Int,Active:Boolean,noOfusers:Int) extends  Actor {
    var frnds: ArrayBuffer[Int]=new ArrayBuffer[Int]()
    var totalpost: Int = 0
    var totalFrnds: Int = 0
    var totalpic: Int = 0
    var myKeyPair:KeyPair=kpg.generateKeyPair()
    var privateKey=myKeyPair.getPrivate
    var publicKey=myKeyPair.getPublic
    var aesKey=kgen.generateKey()
    var secureToken:SecureRandom= new SecureRandom()
    var token:String= ""
    PublicKeyStore.KeyMap.put(id,publicKey)
    var Settings:Int=0

    var allowedUsers:ArrayBuffer[Int]= new ArrayBuffer[Int]()

    var IVPost:IvParameterSpec = generateIV()
    var IVImage:IvParameterSpec = generateIV()

    class keys {
      var AESKey:Array[Byte] = null
      var IVParamPost:IvParameterSpec = null
      var IVParamImage:IvParameterSpec = null
      def keys() {
      }
    }
    var frndsKeysStore:java.util.HashMap[Int,keys] = new util.HashMap[Int,keys]()

    def encryptAESKey(aeskey:SecretKey,mypair:PublicKey): Array[Byte]= {
      var c:Cipher= Cipher.getInstance("RSA")
      c.init(Cipher.ENCRYPT_MODE,mypair)
      var keyEncoded= c.doFinal(aeskey.getEncoded)
      return keyEncoded
    }

    def decryptAESKey(encodedKey:Array[Byte],mypair:PrivateKey): SecretKey={
      var dec:Cipher =Cipher.getInstance("RSA")
      dec.init(Cipher.DECRYPT_MODE, mypair);
      var  key = new SecretKeySpec(dec.doFinal(encodedKey), "AES");
      return  key;
    }

    def EncryptUsingAES(aeskey:SecretKey,myMessage:String,IV:IvParameterSpec): Array[Byte]= {
      var mymsgByte:Array[Byte]=myMessage.getBytes("UTF-8")
      var encryptCipher:Cipher=Cipher.getInstance("AES/CBC/PKCS5Padding")
      encryptCipher.init(Cipher.ENCRYPT_MODE, aeskey,IV)
      var ct = encryptCipher.doFinal(mymsgByte);
      return ct
    }

    def DecryptUsingAES(aeskey:SecretKey,IV:IvParameterSpec,stringByte:Array[Byte]): String = {
      var decryptCipher:Cipher=Cipher.getInstance("AES/CBC/PKCS5Padding")
      decryptCipher.init(Cipher.DECRYPT_MODE, aeskey, IV);
      var plainByte:Array[Byte] = decryptCipher.doFinal(stringByte);
      val plainText = new String(plainByte,"UTF-8")
      return plainText;
    }

    def createCookie(): HttpCookie = {
      var instance:Signature = Signature.getInstance("SHA1withRSA");
      instance.initSign(privateKey);
      instance.update(stringToBytes(token));
      var signature = instance.sign();

      var myencodedToken= bytesToString(signature)
      var mystr1 = myencodedToken.replace("\u000d", "")
      mystr1 = mystr1.replace("\u000a", "")
      val httpCookie:HttpCookie = new HttpCookie(name = "sessionCookie" , content = mystr1)
      return httpCookie
    }

    def encryptImage(path: String , name: String) :String = {
      val filename = path + File.separator + name + ".jpg"
      val file = new File(filename)
      val in = new FileInputStream(file)
      val bytes = new Array[Byte](file.length.toInt)
      in.read(bytes)
      in.close()
      var encoded = ""
      encoded =
        new BASE64Encoder()
          .encode(bytes)
          .replace("\n", "")
          .replace("\r", "")
      in.close()
      var encryptedByte = EncryptUsingAES(aesKey,encoded,IVImage)
      var encryptedString = bytesToString(encryptedByte)
      encryptedString =
        encryptedString.replace("=","`")
          .replace("\u000d","")
          .replace("\u000a","")
      encryptedString
    }

    def bytesToString(bytes: Array[Byte]) : String = {
      return new BASE64Encoder().encode(bytes);
    }

    def stringToBytes(string :String) : Array[Byte] = {
      return new BASE64Decoder().decodeBuffer(string);
    }

    def receive = {
      case "Start" => {
        var r = publicKey.getEncoded
        var mystr = bytesToString(r)
        var randomSettings=Random
        // Settings=  randomSettings.nextInt(3)

        Settings=1
        var mybytes = stringToBytes(mystr)
        var publicKeySpec: EncodedKeySpec = new X509EncodedKeySpec(mybytes);
        var publicKey2: PublicKey = KeyFactory.getInstance(publicKey.getAlgorithm).generatePublic(publicKeySpec);

        var same = publicKey.equals(publicKey2)
        var mystr1 = mystr.replace("\u000d", "_")
        mystr1 = mystr1.replace("\u000a", "~")

        var newEncoding = stringToBytes(mystr1)

        //println(id + " --->   MY PUBLIC KEY CLIENT SIDE-->" + publicKey)
        var result =  pipeline(Post("http://localhost:8080/intialize/mine?userId=" + id + "&myPublicKey=" + mystr1 + "&setng="+Settings))

        result.foreach { response =>
          println(s"Request Initialize user completed with status ${response.status} and content:\n${response.entity.asString}")
          var json = new JSONObject(response.entity.asString)
          token = json.getString("token")
          println(token)
          self ! "done"
        }
      }

      case "done" =>
        self ! "ProfilePic"
        println("Added Profile Picture as well..!!")
        clientmaster ! "doneInit"

      case "setSettings"=>
        if(Settings==0) {
          for(i<-0 until noOfusers) {
            //All USERS CAN ACCESS THE DATA
            allowedUsers(i) = i
            var key: PublicKey = PublicKeyStore.KeyMap.get(i)
            var encryptedAES = encryptAESKey(aesKey, key)
            facebookUsers(i) ! AddMyKeys(id, encryptedAES, IVImage, IVPost)
            println("--ADDED KEy--")
          }
          clientmaster ! "doneSettings"
        }
        else if(Settings==2) {
          allowedUsers+=id
          var encryptedAES= encryptAESKey(aesKey,publicKey)
          var key1 = new keys()
          key1.AESKey = encryptedAES
          key1.IVParamImage = IVImage
          key1.IVParamPost = IVPost
          frndsKeysStore.put(id,key1)
          clientmaster ! "doneSettings"
        }
        else
          clientmaster ! "doneSettings"

      case SeeProfile(check) =>
   //     println(" SEE PROFILE")
        var r =Random
        var httpCookie=createCookie()
        var userId=0
        if(check==1) {
          if (frnds.size > 0)
            userId = frnds(0)
          else
            userId = r.nextInt(noOfusers)
        }
        else userId=r.nextInt(noOfusers)


        val postResult = pipeline(Get("http://localhost:8080/"+ id + "/getprofile/" + userId).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
        postResult.foreach { response =>
          var postslist: ArrayBuffer[String] = new ArrayBuffer[String]()
          if (response.entity.asString.contains("Not Authorized")) {
            println("Not Authorized")
          }
          else {
            var b = new JSONObject(response.entity.asString)
            var a = b.getJSONArray("post").get(0).toString
            var flist = b.getJSONArray("friends").get(0).toString
            var pic = b.getString("profilePic")
            println(s"Request getProfile completed with status ${response.status}")
            var substr = a.substring(1, a.length - 1)
            var Ids = substr.split(",")
            //   println(id + " SPLITTES ARRAY SIZE" + Ids.size)

            for (i <- 0 until Ids.size) {
              if (Ids(i) != "") {
                postslist += Ids(i).substring(1, Ids(i).length - 1) + "=="

              }
            }
            var dstring: ArrayBuffer[String] = new ArrayBuffer[String]()
            var myfrndKey = frndsKeysStore.get(userId)
            var decryptedPic: String = ""
            if (myfrndKey != null) {
              var myFrndsKey = decryptAESKey(myfrndKey.AESKey, privateKey)
              for (i <- 0 until postslist.size) {
                postslist(i) = postslist(i).replace(" ", "+")
                dstring += DecryptUsingAES(myFrndsKey, myfrndKey.IVParamPost, stringToBytes(postslist(i)))
                //println("DSTRINGGGGGGG--->"+ dstring)
              }
              decryptedPic = DecryptUsingAES(myFrndsKey, myfrndKey.IVParamImage, stringToBytes(pic))
            }

            println("*************USER :" + userId + " PROFILE****** : Requested by--->>> " + id)
            println("name --> " + " user: " + userId)
            println("FriendList --> " + flist)
            if (myfrndKey != null) {
              for (i <- 0 until dstring.size)
                println("Posts --> " + dstring(i))
              println("Profile Pic: ---> " + decryptedPic)
            }
            else { println("NOT AUTHORIZED to View the Posts and Profile Picture..!!!! ")}
          }
        }

      case post(noOfusrs) =>
        totalpost += 1
        var r = Random
        var stringindex = r.nextInt(5)
        var frndId=0
        var httpCookie= createCookie()
        if(frnds.size>0)
          frndId=frnds(0)
        else
          frndId = Random.nextInt(noOfusers)
        if(frndsKeysStore.containsKey(frndId)==true) {
          var key1 = frndsKeysStore.get(frndId)
          var symKey = key1.AESKey
          var frndSymKey: SecretKey = decryptAESKey(symKey, privateKey)
          var encryptdData = EncryptUsingAES(frndSymKey, "Post by " + id+"for "+frndId, key1.IVParamPost )
          var try1= bytesToString(encryptdData)
          //var try2=stringToBytes(try1)
          // println("THE last string"+try2.toString)
          var i= encryptdData.size-1
          // println("main --->"+ encryptdData.toString+" OUTPUT"+ try2.toString)
          //var decrypt= DecryptUsingAES(frndSymKey,IVparam,try2)
          //println(frndId+"ORIGINAL->" + encryptdData + "NOW CONVERETED String DECRYPTED DATA"+decrypt)
          pipeline(Post("http://localhost:8080/addPost/mine?userId=" + id + "&frndId=" + frndId + "&msg=" + try1.substring(0,try1.length-2)).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
        }
        sender ! "donePost"

      case "acceptRequest"=>
     //   println("accepting requestssss----->>")
        var httpCookie=createCookie()
        val FriendResult= pipeline(Get("http://localhost:8080/AcceptRequest/" + id).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
        FriendResult.foreach { response =>
          var b = new JSONObject(response.entity.asString)
          println(s"Request AccpetRequest completed with status ${response.status} and content:\n${response.entity.asString}")
          //  println("data send" + b.getJSONArray("friends").get(0).toString)
          var lngth = b.getJSONArray("friends").get(0).toString.length
          var substr = b.getJSONArray("friends").get(0).toString.substring(1, lngth - 1)
          var Ids = substr.split(",")
          for (i <- 0 until Ids.size) {
            if (Ids(i) != "") {

              frnds += Ids(i).toInt

            }
          }
          if(Settings==1)
            self ! maketable(frnds)
        }

      case maketable(frnds)=>
        implicit val timeout = Timeout(100 second)

        for (i <- 0 until frnds.size) {
          var key: PublicKey = PublicKeyStore.KeyMap.get(frnds(i))
          //    println("PUBLIC KEY" + key)
          var encryptedAES = encryptAESKey(aesKey, key)
          // println("The frnd ID------>" + frnds(i) + "Encrypted Key-------->" + encryptedAES)
          val future = facebookUsers(frnds(i)) ? AddKeys(id, encryptedAES,IVImage,IVPost)
          val s = Await.result(future, timeout.duration).asInstanceOf[SendKey]
          if(s != null ) {
            var key1 = new keys()
            key1.AESKey = s.encryptedAES
            key1.IVParamImage = s.iV1
            key1.IVParamPost = s.iV2
            frndsKeysStore.put(frnds(i), key1)
          }
        }

        clientmaster ! "donefrnd"

      case AddKeys(id,encryptedAES,iV1,iV2)=>
        //   println("ADDING THE AES KEY")
        var key1 = new keys()
        key1.AESKey = encryptedAES
        key1.IVParamImage = iV1
        key1.IVParamPost = iV2
        frndsKeysStore.put(id,key1)
        var key: PublicKey = PublicKeyStore.KeyMap.get(id)
        //    println(id+"  send back PUBLIC KEY  " + key)
        var encryptedAES1 = encryptAESKey(aesKey, key)
        //   println("The send back ID------>"+ id+ "Encrypted Key-------->"+ encryptedAES1)
        if(Settings==1)
        sender ! SendKey(encryptedAES1,IVImage,IVPost)
        else sender ! null
      //sender ! encryptedAES1


      case "ProfilePic" =>
        var ProfilePath = System.getProperty("user.dir") + File.separator + "Profile";
        var httpCookie=createCookie()
        val pic: String = encryptImage(ProfilePath, ((id%4)+1).toString)
        var result = pipeline(Post("http://localhost:8080/postProfilePic?userId=" + id + "&image=" + pic).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
        result.foreach { response =>
          println(s"Request ProfilePic completed with status ${response.status}")
        }

      case "PostAlbum" =>
        var albumPath = System.getProperty("user.dir") + File.separator + "Album" + File.separator + ((id%4)+1);
        val httpCookie:HttpCookie = createCookie()
        for(i<-1 until 4) {
          val pic: String = encryptImage(albumPath, i.toString)
          var result = pipeline(Post("http://localhost:8080/postAlbum?userId=" + id + "&image=" + pic).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
          result.foreach { response =>
            println(s"Request PostAlbum completed with status ${response.status}")
            self ! "AlbumDone"
          }
        }

      case "AlbumDone" =>
        totalpic +=1
        if(totalpic >= 3)
          clientmaster ! "AlbumPostDone"

      case "getAlbum" =>
        val httpCookie:HttpCookie = createCookie()
        val albumResult = pipeline(Get("http://localhost:8080/getAlbum/" + id).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
        albumResult.foreach { response =>
          println(s"Request getProfile completed with status ${response.status}")
          if(response.entity.asString!=null || response.entity.asString !="" ) {
            val json = new JSONObject(response.entity.asString)
            val pic = json.getJSONArray("pic")
            val len = pic.get(0).toString.length
            val pics = pic.get(0).toString.substring(1, len - 1).split(',')
            var decryptedPics = new ArrayBuffer[String]()
            println("User Id Album: " + id)
            for (i <- 0 until pics.length) {
              pics(i) = pics(i).substring(1, pics(i).length - 1)
              var d = DecryptUsingAES(aesKey, IVImage, stringToBytes(pics(i)))
              decryptedPics += d
              print("Pic " + i + ":")
              println(d)
            }
          }
          else {println("No ALBUM")}

        }

        sender ! "MoreProfiles"

      case RaiseFrndRequest(noOfusrs) =>
        var httpCookie=createCookie()
        var frndId = Random.nextInt(noOfusers)
        val FriendResult = pipeline(Get("http://localhost:8080/" + id + "/friendRequest/" + frndId).withHeaders(Cookie(httpCookie))~> addHeader(Cookie(httpCookie)))
        FriendResult.foreach { response =>
          println(s"Request addFriend completed with status ${response.status} and content:\n${response.entity.asString}")
        }
        sender ! "RaisedRequest"

      case "donefrnd"=>
        println("NOW THIS IS HAPPENING")
    }
  }

  object Global {
    var Postarray = new Array[String](100)
    var i=0
    var filename = "C:/Post/FBPost.txt"
    for(line <- scala.io.Source.fromFile(filename).getLines()){
      Postarray(i) =line
      i+=1
    }
  }

  object PublicKeyStore {
    var KeyMap:java.util.HashMap[Int, PublicKey]= new util.HashMap[Int,PublicKey]()
  }
}

