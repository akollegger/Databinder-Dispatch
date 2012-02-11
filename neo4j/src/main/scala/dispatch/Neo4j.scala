package dispatch.neo4j

import dispatch._
import json._
import JsHttp._
import java.lang.String
import java.net.URL

/**Extractors for Neo4j node metadata.
    Extend with your own document properties. */
trait NodeMeta extends Js
{
  val self = 'self ? str
}

/**Extractors for Neo4j node metadata.
    Use this object for direct access to Id extractors. */
object NodeMeta extends NodeMeta

class Retry[A]
{
  private var v: Option[A] = None

  def retry(f: () => Option[A]) =
  {
    v = v.orElse(f())
    v
  }
}

trait DiscoveryEndpoint extends Request with Js
{
  var _meta: Option[Map[JsString, JsValue]] = None

  def meta(implicit http: Http) =
  {
    _meta = _meta.orElse({
      Some(http(this ># obj).self)
    })
    _meta
  }

  def discover[B](key: String)(implicit http: Http) =
  {
    meta.flatMap({
      _.get(JsString(key))
    }).flatMap(_ match
    {
      case b: B => Some(b);
      case _ => None
    })
  }
}

/**Factory for a Neo4j Request host with common parameters */
object Neo4j
{
  def apply(): Neo4j = this("127.0.0.1")

  def apply(hostname: String): Neo4j = Neo4j(hostname, 7474)
}

/**Requests for a particular Neo4jDB host. */
case class Neo4j(hostname: String, port: Int) extends Request(:/(hostname, port)) with DiscoveryEndpoint
{
  def data(implicit http: Http) = discover[JsString]("data").map(s => new GraphData(s.self))

  def management(implicit http: Http) = discover[JsString]("management").map(s => new DbManagement(s.self))

  // _management.retry(() => discover(http).get("management").map(new DbManagement(_)))
}

/**Requests on a particular database and Neo4jDB host. */
case class GraphData(endpoint: String) extends Request(endpoint) with DiscoveryEndpoint
{
  def referenceNode(implicit http: Http) = discover[JsString]("reference_node").map(s => new Node(s.self))

  //  def relationshipTypes(implicit http: Http) = _relationshipTypes.retry(() => discover(http).get("relationship_types").map(new Request(_)))
  def relationshipTypes(implicit http: Http) = discover[JsString]("relationship_types").map(s => new Request(s.self))

  //  def extensionsInformation(implicit http: Http) = _extensionsInformation.retry(() => discover(http).get("extensions_info").map(new Request(_)))
  def extensionsInformation(implicit http: Http) = discover[JsString]("extensions_info").map(s => new Request(s.self))

  //  def nodeIndex(implicit http: Http) = _nodeIndex.retry(() => discover(http).get("node_index").map(new Request(_)))
  def nodeIndex(implicit http: Http) = discover[JsString]("node_index").map(s => new Request(s.self))

  def extensions(implicit http: Http) = discover[JsObject]("extensions").map(s => ExtensionsMap(s))

  def createNode(implicit http: Http) = discover[JsString]("node").map(s => new Request(s.self))

}

case class DbManagement(endpoint: String) extends Request(endpoint) with DiscoveryEndpoint
{
  def services(implicit http: Http) = discover[JsObject]("services").map(s => Services(s))
}

object Node
{
  def apply(js: JsObject):Option[Node] = {
    js.self.get(JsString("self")).flatMap(_ match {
      case JsString(s) => {
        val newNode = new Node(s.self)
        newNode._meta = Some(js.self)
        Some(newNode)
      }
      case _ => None
    })
  }
}
case class Node(endpoint: String) extends Request(endpoint) with DiscoveryEndpoint
{
  def self(implicit http: Http) = discover[JsString]("self").map(s => new Request(s.self))

  def data(implicit http: Http) = discover[JsObject]("data")

  def properties(implicit http: Http) = discover[JsString]("properties").map(s => new Request(s.self))

  def outgoingRelationships(implicit http: Http) = discover[JsString]("outgoing_relationships").map(s => new Request(s.self))

  def incomingRelationships(implicit http: Http) = discover[JsString]("incoming_relationships").map(s => new Request(s.self))

  def allRelationships(implicit http: Http) = discover[JsString]("all_relationships").map(s => new Request(s.self))

  def createRelationship(implicit http: Http) = discover[JsString]("create_relationship").map(s => new Request(s.self))

  def delete = DELETE >|
}

object Services
{
  def apply(js: JsObject) =
  {
    js.self withFilter
        {
          case (k, v: JsString) => true
          case _ => false
        } map
        {
          case (k, v: JsString) => (k.self, new Request(v.self))
          case _ => throw new IllegalStateException("Inexplicable state of filtered Services map: " + js)
        }
  }
}

object ExtensionsMap
{
  def apply(js: JsObject) =
  {
    js.self.map
    {
      case (k, v) =>
      {
        (k.self -> ServerExtension(k, v))
      }
    }
  }
}

case class ServerExtension(name: String, methods: Map[String, Request])
{

}

object ServerExtension
{
  def apply(name: JsString, methods: JsValue) =
  {
    new ServerExtension(name.self,
      methods match
      {
        case j: JsObject =>
        {
          j.self withFilter
              {
                case (k, v: JsString) => true
                case _ => false
              } map
              {
                case (k, v: JsString) => (k.self, new Request(v.self))
                case _ => throw new IllegalStateException("Inexplicable state of filtered map: " + methods)
              }
        }
        case _ => Map[String, Request]()
      }
    )
  }
}
