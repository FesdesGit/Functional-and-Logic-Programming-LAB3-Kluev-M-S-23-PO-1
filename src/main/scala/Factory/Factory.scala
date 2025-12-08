package Factory

import singletonfabric.Singleton
import State.State

case class FactoryState(
                         currentFactory: Singleton = Singleton.Singleton0,
                         useFactory0: Boolean = true,
                         operationCount: Int = 0
                       )

object FactoryOperations {

  def pressFactoryState0: State[FactoryState, Singleton] = State { state =>
    val newFactory = if (state.useFactory0) Singleton.Singleton0 else Singleton.Singleton1
    val newState = state.copy(
      currentFactory = newFactory,
      useFactory0 = true,
      operationCount = state.operationCount + 1
    )
    (newState, newFactory)
  }

  def pressFactoryState1: State[FactoryState, Singleton] = State { state =>
    val newFactory = if (!state.useFactory0) Singleton.Singleton1 else Singleton.Singleton0
    val newState = state.copy(
      currentFactory = newFactory,
      useFactory0 = false,
      operationCount = state.operationCount + 1
    )
    (newState, newFactory)
  }

  def getCount: State[FactoryState, Int] = State(state => (state, state.operationCount))

  def resetCount: State[FactoryState, Unit] =
    State.modify(state => state.copy(operationCount = 0))
}