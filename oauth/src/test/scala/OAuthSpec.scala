import org.specs._

object OAuthSpec extends Specification {
  import dispatch._
  import oauth._
  import Request._
  import OAuth._
  
  val svc = :/("term.ie") / "oauth" / "example"
  val consumer = Consumer("key", "secret")
  
  "OAuth test host" should {
    "echo parameters from protected service" in {
      val h = new Http
      val request_token = h(svc / "request_token.php" <@ consumer as_token)
      val access_token = h(svc / "access_token" <@ (consumer, request_token) as_token)
      val payload = Map("identité" -> "caché", "identity" -> "hidden", "アイデンティティー" -> "秘密", 
        "pita" -> "-._~*")
      h(
        svc / "echo_api.php" <<? payload <@ (consumer, access_token) >% {
          _ must_== (payload)
        }
      )
    }
  }
}
