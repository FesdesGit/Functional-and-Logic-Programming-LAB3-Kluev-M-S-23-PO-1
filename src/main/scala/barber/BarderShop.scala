package barber

import singletonfabric.Singleton
import Factory.{FactoryState, FactoryOperations}
import proxy._
import State.State

import scala.collection.immutable.Queue
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.Random

case class Customer(id: Int, name: String)
case class BarberShopState(
                            waitingCustomers: Queue[Customer] = Queue.empty,
                            isSleeping: Boolean = true,
                            availableSeats: Int = 3,
                            cacheState: CacheSystemState = CacheSystemState(),
                            factoryState: FactoryState = FactoryState(),
                            cutCount: Int = 0
                          )
class ConcurrentQueue[T] {
  private var queue = Queue.empty[T]

  def enqueue(item: T): Unit = synchronized {
    queue = queue.enqueue(item)
  }

  def dequeue(): Option[T] = synchronized {
    if (queue.isEmpty) None
    else {
      val (item, newQueue) = queue.dequeue
      queue = newQueue
      Some(item)
    }
  }

  def size: Int = synchronized {
    queue.size
  }

  def isEmpty: Boolean = synchronized {
    queue.isEmpty
  }
}

class BarberShop(seats: Int) {
  private val customerQueue = new ConcurrentQueue[Customer]()
  private var sleeping = true
  private var running = true

  def enter(customer: Customer): Boolean = synchronized {
    println(s"${customer.name} вошёл в барбершоп.")

    if (customerQueue.size >= seats) {
      println(s"${customer.name} не нашёл свободных мест и ушёл.")
      false
    } else {
      customerQueue.enqueue(customer)
      println(s"${customer.name} сел в кресло для ожидания.")

      if (sleeping) {
        println(s"${customer.name} разбудил парикмахера.")
        sleeping = false
        notifyAll()
      }
      true
    }
  }

  def nextCustomer(): Option[Customer] = synchronized {
    while (customerQueue.isEmpty && running) {
      println("Парикмахер спит, ожидает клиентов...")
      sleeping = true
      wait()
    }

    if (!running) return None

    sleeping = false
    customerQueue.dequeue()
  }

  def stop(): Unit = synchronized {
    running = false
    notifyAll()
  }

  def isRunning: Boolean = synchronized { running }
}

class Barber(shop: BarberShop, n: Int) extends Runnable {
  private val random = new Random()

  override def run(): Unit = {
    while (shop.isRunning) {
      shop.nextCustomer() match {
        case Some(customer) =>
          cutHair(customer)
        case None =>
          return
      }
    }
  }

  private def cutHair(customer: Customer): Unit = {
    println(s"Парикмахер начал стричь ${customer.name}...")

    try {
      Thread.sleep(1000)

      val initialState = BarberShopState(
        availableSeats = n,
        cacheState = CacheSystemState(),
        factoryState = FactoryState()
      )

      val operationsCount = random.nextInt(19)
      val operations = (1 to operationsCount).map { i =>
        val useFactory0 = random.nextBoolean()
        val count = random.nextInt(15) + 1

        (useFactory0, count)
      }

      var currentState = initialState

      operations.foreach { case (useFactory0, count) =>
        val (newFactoryState, singleton) = if (useFactory0) {
          FactoryOperations.pressFactoryState0.run(currentState.factoryState)
        } else {
          FactoryOperations.pressFactoryState1.run(currentState.factoryState)
        }

        val decorators = List(
          CacheDecorator.countDecorator,
          CacheDecorator.timestampDecorator
        )

        val (newCacheState, messages) = CachePipeline.processWithDecorators(
          singleton,
          count,
          decorators
        ).run(currentState.cacheState)

        currentState = currentState.copy(
          factoryState = newFactoryState,
          cacheState = newCacheState
        )

        messages.foreach(msg => println(s"  [${msg.level}] ${msg.message}"))
      }

      val (finalFactoryState1, singleton1) =
        FactoryOperations.pressFactoryState0.run(currentState.factoryState)

      val (finalCacheState1, messages1) = CachePipeline.processWithDecorators(
        singleton1,
        1,
        List(CacheDecorator.countDecorator)
      ).run(currentState.cacheState)

      val (finalFactoryState2, singleton2) =
        FactoryOperations.pressFactoryState1.run(finalFactoryState1)

      val (finalCacheState2, messages2) = CachePipeline.processWithDecorators(
        singleton2,
        1,
        List(CacheDecorator.countDecorator)
      ).run(finalCacheState1)

      messages1.foreach(msg => println(s"  [${msg.level}] ${msg.message}"))
      messages2.foreach(msg => println(s"  [${msg.level}] ${msg.message}"))

      println(s"Парикмахер закончил стричь ${customer.name}.")
      println(s"Статистика: L1=${finalCacheState2.l1.size}, L2=${finalCacheState2.l2.size}, L3=${finalCacheState2.l3.size}")

    } catch {
      case e: InterruptedException =>
        Thread.currentThread().interrupt()
        println(s"Стрижка ${customer.name} прервана.")
    }
  }
}

class CustomerRunnable(id: Int, shop: BarberShop) extends Runnable {
  override def run(): Unit = {
    val customer = Customer(id, s"Клиент-$id")
    shop.enter(customer)
  }
}