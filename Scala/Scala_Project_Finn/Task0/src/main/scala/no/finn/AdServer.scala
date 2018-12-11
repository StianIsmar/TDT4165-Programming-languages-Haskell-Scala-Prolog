package no.solution

import no.finn.common._

trait AdServer extends Server with Database[String] {

  private def addAd(): Unit = {
    val adData: String   = readLine("Enter ad data: ")
    val insertedId: AdId = insertInDatabase(adData)
    printConsole(s"Inserted ad with id: $insertedId")
  }

  private def readAd(): Unit = {
    val adId: AdId = AdId(readLine("Enter adId: ").toLong)
    printConsole(getFromDatabase(adId).get)
  }

  def run(): Unit = {
    var mode: Mode = UnknownMode

    while (mode != QuitMode) {
      mode = Mode.fromString(readLine("Select mode: quit, add, read: "))

      mode match {
        case AddMode     => addAd()
        case ReadMode    => readAd()
        case UnknownMode => printConsole("unknown mode")
        case QuitMode    => printConsole("Goodbye")
      }
    }
  }
}

object MainSolution extends AdServer with RealConsole {
  def main(args: Array[String]): Unit =
    run()
}