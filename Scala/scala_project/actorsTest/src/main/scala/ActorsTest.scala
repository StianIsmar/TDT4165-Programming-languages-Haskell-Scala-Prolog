import akka.pattern._

class Pongy extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "ping" =>
      log.info("Got a ping -- ponging back!")
      sender ! "pong"
      context.stop(self)
  }
  override def postStop() = log.info("pongy going down")
} 