package no.finn

sealed trait Ad {
  def toConsoleString: String
}

object CarAd(regNr: String, price: Long) extends Ad {
  override def toConsoleString: String = s"car $regNr $price"
}

object JobAd(company: String, monthlySalary: Long) extends Ad {
  override def toConsoleString: String = s"job $company $monthlySalary"
}

object UnknownAdType extends Ad {
  override def toConsoleString: String = "UnknownAd"
}



object Ad {
  private def splitString(s: String): List[String] = s.split(' ').toList

  def fromString(s: String): Ad = {
    val stringList: List[String] = splitString(s)

    if (stringList(0) == "car") {
      Car(stringList(1),stringList(2).toLong)

    } else if (stringList(0) == "job") {
      JobAd(stringList(1),stringList(2).toLong)

    } else {
      UnknownAdType
    }
  }
}
