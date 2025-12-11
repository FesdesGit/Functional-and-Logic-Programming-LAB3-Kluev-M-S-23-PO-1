import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Random, Try, Success, Failure}
import scala.collection.immutable.Queue

case class Customer(id: Int, name: String, arrivalTime: Long = System.currentTimeMillis())

case class ShopStats(
                      totalCustomers: Int = 0,
                      servedCustomers: Int = 0,
                      rejectedCustomers: Int = 0,
                      averageWaitTime: Double = 0.0,
                      cacheStats: CacheSystemState = CacheSystemState()
                    )

class AsyncQueue[A](maxSize: Int)(implicit ec: ExecutionContext) {
  private var queue = Queue.empty[A]
  private val promises = scala.collection.mutable.Queue.empty[Promise[Option[A]]]
  private val lock = new AnyRef

  def enqueue(item: A): Future[Boolean] = Future {
    lock.synchronized {
      if (queue.size < maxSize) {
        queue = queue.enqueue(item)
        while (promises.nonEmpty && queue.nonEmpty) {
          val promise = promises.dequeue()
          val (element, newQueue) = queue.dequeue
          queue = newQueue
          promise.success(Some(element))
        }
        true
      } else {
        false
      }
    }
  }

  def dequeue(): Future[Option[A]] = {
    val promise = Promise[Option[A]]()

    Future {
      lock.synchronized {
        if (queue.nonEmpty) {
          val (element, newQueue) = queue.dequeue
          queue = newQueue
          promise.success(Some(element))
        } else {
          promises.enqueue(promise)
        }
      }
    }.flatMap(_ => promise.future)
  }

  def size: Future[Int] = Future {
    lock.synchronized {
      queue.size
    }
  }

  def isEmpty: Future[Boolean] = size.map(_ == 0)
}

class BarberShopService(seats: Int) {
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val waitingRoom = new AsyncQueue[Customer](seats)
  private var isOpen = true
  private var totalServed = 0
  private var totalRejected = 0
  private var waitTimes = Vector.empty[Long]

  private var cacheState = CacheSystemState()
  private var factoryState = FactoryState()

  def customerArrives(customer: Customer): Future[Boolean] = {
    if (!isOpen) {
      Future.successful(false)
    } else {
      waitingRoom.enqueue(customer).map { success =>
        if (!success) {
          totalRejected += 1
        }
        success
      }
    }
  }

  def startBarber(): Future[Unit] = {
    def barberLoop(): Future[Unit] = {
      if (!isOpen) {
        Future.successful(())
      } else {
        for {
          maybeCustomer <- waitingRoom.dequeue()
          _ <- maybeCustomer match {
            case Some(customer) =>
              for {
                _ <- Future {
                  println(s"Парикмахер начал стричь ${customer.name}")
                }
                _ <- serveCustomer(customer)
                _ = {
                  totalServed += 1
                  val waitTime = System.currentTimeMillis() - customer.arrivalTime
                  waitTimes = waitTimes :+ waitTime
                }
              } yield ()
            case None =>
              Future {
                Thread.sleep(1000)
              }
          }
          _ <- barberLoop()
        } yield ()
      }
    }

    barberLoop()
  }

  private def serveCustomer(customer: Customer): Future[Unit] = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val random = new Random(customer.id)
    val operationsCount = random.nextInt(19)

    val operations = (1 to operationsCount).map { i =>
      val useSingleton0 = random.nextBoolean()
      val count = random.nextInt(15) + 1
      (useSingleton0, count)
    }.toList

    val processFutures = operations.map { case (useSingleton0, count) =>
      for {
        factoryResult <- FactoryOperations
          .createSingleton(useSingleton0)
          .run(factoryState)
        (newFactoryState, singleton) = factoryResult

        _ = factoryState = newFactoryState

        cacheResult <- CachePipeline
          .processWithDecorators(
            singleton,
            count,
            List(CacheDecorator.countDecorator)
          )
          .run(cacheState)
        (newCacheState, messages) = cacheResult

        _ = cacheState = newCacheState

        _ = messages.foreach { msg =>
          println(s"  [${customer.name}] [${msg.level}] ${msg.message}")
        }

      } yield ()
    }

    Future.sequence(processFutures).map { _ =>
      Thread.sleep(500 + random.nextInt(500))
      println(s"Парикмахер закончил стричь ${customer.name}")
    }
  }

  def generateCustomers(total: Int, intervalMillis: Int): Future[Unit] = {
    def generateOne(i: Int): Future[Unit] = {
      if (i > total || !isOpen) {
        Future.successful(())
      } else {
        val customer = Customer(i, s"Клиент-$i")
        for {
          success <- customerArrives(customer)
          _ = {
            if (success) {
              println(s"${customer.name} вошел и сел ждать")
            } else {
              println(s"${customer.name} ушел - нет мест")
            }
          }
          _ <- Future {
            Thread.sleep(intervalMillis)
          }
          _ <- generateOne(i + 1)
        } yield ()
      }
    }

    generateOne(1)
  }

  def getStats(): Future[ShopStats] = Future {
    val avgWaitTime = if (waitTimes.nonEmpty) {
      waitTimes.sum.toDouble / waitTimes.size
    } else 0.0

    ShopStats(
      totalCustomers = totalServed + totalRejected,
      servedCustomers = totalServed,
      rejectedCustomers = totalRejected,
      averageWaitTime = avgWaitTime,
      cacheStats = cacheState
    )
  }

  def close(): Unit = {
    isOpen = false
  }

  def isShopOpen: Boolean = isOpen
}

class SimulationService {
  implicit val ec: ExecutionContext = ExecutionContext.global

  def runSimulation(seats: Int, totalCustomers: Int): Future[ShopStats] = {
    val shop = new BarberShopService(seats)

    val barberFuture = shop.startBarber()

    val customersFuture = shop.generateCustomers(totalCustomers, 500)

    for {
      _ <- customersFuture
      _ <- Future {
        Thread.sleep(5000)
      }
      stats <- shop.getStats()
      _ = shop.close()
      _ <- barberFuture
    } yield stats
  }
}