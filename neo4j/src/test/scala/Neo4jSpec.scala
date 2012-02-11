import java.lang.String
import org.specs._
import scala.collection.immutable.Map

object Neo4jSpec extends Specification
{

  import dispatch._
  import dispatch.neo4j._
  import dispatch.json._
  import JsHttp._

  implicit val http = new Http
  val localNeo4j = Neo4j()
  // these tests expect Neo4jDB to be running at 127.0.0.1 on port 7474
  val UrlPattern = """^http:\/\/[\w\.]+(:\d+)?(/.*)?$"""

  "A Neo4j server" should
      {

        doFirst
        {
          /* start a Neo4j test server */
        }

        doLast
        {
          /* stop the Neo4j test server */
        }

        "reveal the graph data endpoint" in
            {
              http(localNeo4j as_str) must find("""\"data\"\s?:\s?\"http:\/\/[^"]+\"""")
            }
        "define the graph data endpoint" in
            {
              localNeo4j.data must beSomething
              localNeo4j.data.get.to_uri must notBeNull // actually, will throw an exception if endpoint is not a valid uri
            }
        "define the graph management endpoint" in
            {
              localNeo4j.management must beSomething
              localNeo4j.management.get.to_uri must notBeNull
            }
      }
  "Default graph database" should
      {
        "have discoverable endpoints" in
            {
              val graphData = localNeo4j.data.get

              graphData.referenceNode must beSomething
              graphData.referenceNode.get.to_uri must notBeNull

              graphData.relationshipTypes must beSomething
              graphData.relationshipTypes.get.to_uri must notBeNull

              graphData.extensionsInformation must beSomething
              graphData.extensionsInformation.get.to_uri must notBeNull

              graphData.nodeIndex must beSomething
              graphData.nodeIndex.get.to_uri must notBeNull

              // extensions are a Map[String, ServerExtension]
              graphData.extensions must beSomething
              graphData.extensions must notBeNull

              graphData.createNode must beSomething
              graphData.createNode.get.to_uri must notBeNull
            }

        "create a node and describe how to access it" in
            {
              val graphData = localNeo4j.data.get

              val createdNodeJS: JsObject = http((graphData.createNode.get POST) ># obj)
              createdNodeJS must notBeNull
              createdNodeJS.self.get(JsString("self")) must beSomething
              createdNodeJS.self.get(JsString("self")).get match
              {
                case JsString(s) => s mustMatch (UrlPattern)
                case _ => fail("node.self should be a JsString")
              }

              val createdNode = Node(createdNodeJS).get
              http(createdNode.delete)
            }
      }

  "A Node" should
      {
        "be self aware" in
            {
              val graphData = localNeo4j.data.get
              val referenceNode = graphData.referenceNode
              referenceNode.get.self.get.to_uri must beEqualTo(referenceNode.get.to_uri)
            }
        "have mutable properties" in
            {
              val graphData = localNeo4j.data.get
              val referenceNode = graphData.referenceNode
              val propertiesToPost: JsObject = new JsObject(
                Map(
                  new JsString("name") -> new JsString("Andreas Kollegger"),
                  new JsString("title") -> new JsString("American Hacker")
                )
              )
              http(referenceNode.get.properties.get <<< propertiesToPost.toString >|)
              val retrievedProperties:JsObject = http(referenceNode.get.properties.get ># obj)
              retrievedProperties.self.get(JsString("name")) must beSome(JsString("Andreas Kollegger"))
              retrievedProperties.self.get(JsString("title")) must beSome(JsString("American Hacker"))
              http((referenceNode.get.properties.get DELETE) >|)
            }
      }

  "ExtensionsMap" should
      {
        "be empty when no extension definitions provided" in
            {
              val emptyObject = new JsObject(Map[JsString, JsValue]())
              ExtensionsMap(emptyObject) must beEmpty
            }
        "transform extension definitions to a map of names to ServerExtensions" in
            {
              val extName: String = "fooExtension"
              val extensionDefinitions = new JsObject(Map(
                new JsString(extName) -> JsObject(
                  Map(
                    new JsString("fooMethodA") -> new JsString("http://somewhere/pointing/to/foo/method/A"),
                    new JsString("fooMethodB") -> new JsString("http://somewhere/pointing/to/foo/method/B")
                  )
                )
              ))
              val extensions: Map[String, ServerExtension] = ExtensionsMap(extensionDefinitions)
              extensions must notBeEmpty
              extensions.get(extName) must beSomething
              extensions.get(extName).get.name must be(extName)
              extensions.get(extName).get.methods.get("fooMethodA").get.to_uri must notBeNull
              extensions.get(extName).get.methods.get("fooMethodB").get.to_uri must notBeNull
            }
      }

  "Database management" should
      {
        "define available services" in
            {
              val management = localNeo4j.management.get
              management.services.get must notBeEmpty
            }
      }

}
