package proxy

import singletonfabric.Singleton
import State.State

case class CacheMessage(level: String, message: String, timestamp: Long = System.currentTimeMillis())

case class CacheSystemState(
                             l1: Map[String, String] = Map.empty,
                             l2: Map[String, String] = Map.empty,
                             l3: Map[String, String] = Map.empty,
                             proxyCount: Int = 0,
                             totalOperations: Int = 0,
                             messages: List[CacheMessage] = Nil
                           )

object CacheProxy {

  def process(singleton: Singleton, count: Int): State[CacheSystemState, Unit] =
    State.modify { state =>
      if (count != 0) {
        val message = CacheMessage("Proxy", s"Processing: ${singleton.name}")
        state.copy(
          proxyCount = state.proxyCount + 1,
          totalOperations = state.totalOperations + 1,
          messages = message :: state.messages
        )
      } else state
    }
}

object CacheL3 {

  def process(singleton: Singleton, count: Int): State[CacheSystemState, Unit] =
    State.modify { state =>
      if (count <= 3) {
        state.l3.get(singleton.name) match {
          case Some(data) =>
            val message = CacheMessage("L3", s"Return for ${singleton.name}, data: $data")
            state.copy(
              totalOperations = state.totalOperations + 1,
              messages = message :: state.messages
            )
          case None =>
            val newL3 = state.l3 + (singleton.name -> singleton.value)
            val message = CacheMessage("L3", s"No cache found for: ${singleton.name}")
            state.copy(
              l3 = newL3,
              totalOperations = state.totalOperations + 1,
              messages = message :: state.messages
            )
        }
      } else {
        val newL3 = state.l3 - singleton.name
        val message = CacheMessage("L3", s"Removed ${singleton.name} from L3, count: $count")
        state.copy(
          l3 = newL3,
          totalOperations = state.totalOperations + 1,
          messages = message :: state.messages
        )
      }
    }
}

object CacheL2 {

  def process(singleton: Singleton, count: Int): State[CacheSystemState, Unit] =
    State.modify { state =>
      if (count > 3 && count <= 8) {
        state.l2.get(singleton.name) match {
          case Some(data) =>
            val message = CacheMessage("L2", s"Return for ${singleton.name}, data: $data")
            state.copy(
              totalOperations = state.totalOperations + 1,
              messages = message :: state.messages
            )
          case None =>
            val newL2 = state.l2 + (singleton.name -> singleton.value)
            val message = CacheMessage("L2", s"No cache found for: ${singleton.name}")
            state.copy(
              l2 = newL2,
              totalOperations = state.totalOperations + 1,
              messages = message :: state.messages
            )
        }
      } else {
        val newL2 = state.l2 - singleton.name
        state.copy(
          l2 = newL2,
          totalOperations = state.totalOperations + 1
        )
      }
    }
}

object CacheL1 {

  def process(singleton: Singleton, count: Int): State[CacheSystemState, Unit] =
    State.modify { state =>
      if (count > 8) {
        state.l1.get(singleton.name) match {
          case Some(data) =>
            val message = CacheMessage("L1", s"Return for ${singleton.name}, data: $data")
            state.copy(
              totalOperations = state.totalOperations + 1,
              messages = message :: state.messages
            )
          case None =>
            val newL1 = state.l1 + (singleton.name -> singleton.value)
            val message = CacheMessage("L1", s"No cache found for: ${singleton.name}")
            state.copy(
              l1 = newL1,
              totalOperations = state.totalOperations + 1,
              messages = message :: state.messages
            )
        }
      } else {
        val newL1 = state.l1 - singleton.name
        state.copy(
          l1 = newL1,
          totalOperations = state.totalOperations + 1
        )
      }
    }
}

object CacheDecorator {

  type Decorator = (Singleton, Int, List[CacheMessage]) => List[CacheMessage]

  val countDecorator: Decorator = (singleton, count, messages) =>
    messages :+ CacheMessage("Decorator", s"Count: $count")

  val timestampDecorator: Decorator = (singleton, count, messages) =>
    CacheMessage("Decorator", s"Timestamp: ${java.time.LocalDateTime.now()}") :: messages

  def applyDecorators(
                       baseOperation: (Singleton, Int) => State[CacheSystemState, List[CacheMessage]],
                       decorators: List[Decorator]
                     ): (Singleton, Int) => State[CacheSystemState, List[CacheMessage]] = {
    (singleton, count) =>
      baseOperation(singleton, count).map { messages =>
        decorators.foldLeft(messages) { (msgs, decorator) =>
          decorator(singleton, count, msgs)
        }
      }
  }
}

object CachePipeline {

  def processThroughAllLevels(singleton: Singleton, count: Int): State[CacheSystemState, List[CacheMessage]] = {
    for {
      _ <- CacheProxy.process(singleton, count)
      _ <- CacheL3.process(singleton, count)
      _ <- CacheL2.process(singleton, count)
      _ <- CacheL1.process(singleton, count)
      state <- State.get[CacheSystemState]
    } yield state.messages.reverse
  }

  def processWithDecorators(
                             singleton: Singleton,
                             count: Int,
                             decorators: List[CacheDecorator.Decorator]
                           ): State[CacheSystemState, List[CacheMessage]] = {

    val decoratedOperation = CacheDecorator.applyDecorators(
      processThroughAllLevels,
      decorators
    )

    decoratedOperation(singleton, count)
  }
}

object CacheObserver {

  type Observer = (Singleton, Int, CacheSystemState) => List[CacheMessage]

  val l2Observer: Observer = (singleton, count, state) => {
    if (count > 3 && count <= 8) {
      List(CacheMessage("Observer-L2", s"L2 notified about ${singleton.name}"))
    } else Nil
  }

  val l1Observer: Observer = (singleton, count, state) => {
    if (count > 8) {
      List(CacheMessage("Observer-L1", s"L1 notified about ${singleton.name}"))
    } else Nil
  }

  def notifyObservers(
                       singleton: Singleton,
                       count: Int,
                       state: CacheSystemState,
                       observers: List[Observer]
                     ): List[CacheMessage] = {
    observers.flatMap(observer => observer(singleton, count, state))
  }
}