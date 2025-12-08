import barber.{BarberShop, Barber, CustomerRunnable}
import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executors

object Main {
  def numCheck(): Int = {
    import scala.io.StdIn

    def loop: Int = {
      println("Введите количество объектов (не менее 2):")

      try {
        val n = StdIn.readInt()
        if (n >= 2) n
        else {
          println("Некорректный ввод")
          loop
        }
      } catch {
        case _: NumberFormatException =>
          println("Некорректный ввод")
          loop
      }
    }

    loop
  }

  def main(args: Array[String]): Unit = {
    val n = numCheck()

    val shop = new BarberShop(n)

    val barberThread = new Thread(new Barber(shop, n), "Barber")
    barberThread.start()

    val executor = Executors.newFixedThreadPool(5)

    for (i <- 1 to 10) {
      executor.execute(new CustomerRunnable(i, shop))

      try {
        Thread.sleep(500)
      } catch {
        case e: InterruptedException =>
          Thread.currentThread().interrupt()
      }
    }

    Thread.sleep(15000)

    shop.stop()
    barberThread.interrupt()
    executor.shutdown()

    println("\nБарбершоп закрывается...")
  }
}

object CacheTest {

  def main(args: Array[String]): Unit = {
    println("Тестирование 3-уровневой кэш-системы")

    import singletonfabric.Singleton
    import proxy._

    println("\n1. Тест L3 кэша (count <= 3):")
    val (state1, messages1) = CachePipeline.processThroughAllLevels(
      Singleton.Singleton0,
      2
    ).run(CacheSystemState())

    messages1.foreach(msg => println(s"  ${msg.level}: ${msg.message}"))
    println(s"  L3 кэш: ${state1.l3}")

    println("\n2. Тест L2 кэша (3 < count <= 8):")
    val (state2, messages2) = CachePipeline.processThroughAllLevels(
      Singleton.Singleton1,
      5
    ).run(state1)

    messages2.foreach(msg => println(s"  ${msg.level}: ${msg.message}"))
    println(s"  L2 кэш: ${state2.l2}")

    println("\n3. Тест L1 кэша (count > 8):")
    val (state3, messages3) = CachePipeline.processThroughAllLevels(
      Singleton.Singleton0,
      10
    ).run(state2)

    messages3.foreach(msg => println(s"  ${msg.level}: ${msg.message}"))
    println(s"  L1 кэш: ${state3.l1}")

    println("\n4. Тест с декораторами:")
    val decorators = List(CacheDecorator.countDecorator)
    val (state4, messages4) = CachePipeline.processWithDecorators(
      Singleton.Singleton1,
      3,
      decorators
    ).run(state3)

    messages4.foreach(msg => println(s"  ${msg.level}: ${msg.message}"))

    println("\nФинальное состояние кэша")
    println(s"L1: ${state4.l1.size} элементов")
    println(s"L2: ${state4.l2.size} элементов")
    println(s"L3: ${state4.l3.size} элементов")
    println(s"Всего операций: ${state4.totalOperations}")
  }
}