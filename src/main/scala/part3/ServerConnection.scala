package part3

import scala.language.postfixOps
import scala.util.{Random, Try}

class ServerConnection {

  def connect = "Connected"

}

object ServerConnection {
  val random = new Random(System.nanoTime())

  def apply(host: String, port: String): Option[ServerConnection] =
    if(random.nextBoolean()) Some(new ServerConnection)
    else None
}

val config: Map[String, String] = Map(
  "host" -> "172.33.33.3",
  "port" -> "80"
)

object TryConnect extends App {

  def tryConnect(): String = {
    val host = config.getOrElse("host", throw new BadServerConfiguration)
    val port = config.getOrElse("port", throw new BadServerConfiguration)

    val serverConnection = ServerConnection(host, port)
    val connection = serverConnection.getOrElse(throw new ServerNotConnected)

    connection.connect
  }

  println(Try(tryConnect()))
}

class BadServerConfiguration extends RuntimeException("Bad Server Configuration")
class ServerNotConnected extends RuntimeException("Server Not Connected")