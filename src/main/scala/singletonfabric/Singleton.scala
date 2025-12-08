package singletonfabric

sealed trait Singleton {
  val name: String
  val value: String
}

object Singleton {
  case object Singleton0 extends Singleton {
    val name = "Sin0"
    val value = "This is Singleton0!"
  }

  case object Singleton1 extends Singleton {
    val name = "Sin1"
    val value = "This is Singleton1!"
  }
}

object CounterExample {
  def processWithCounter(singleton: Singleton, count: Int): (String, Int) = {
    val message = s"${singleton.name} called. Count: ${count + 1}"
    (message, count + 1)
  }
}