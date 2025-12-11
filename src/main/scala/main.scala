import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.io.StdIn

object Main {

  def readPositiveInt(prompt: String)(implicit ec: ExecutionContext): Future[Int] = Future {
    def loop: Int = {
      println(prompt)

      Try(StdIn.readInt()) match {
        case Success(n) if n >= 2 => n
        case Success(_) =>
          println("Число должно быть не меньше 2. Попробуйте снова.")
          loop
        case Failure(_) =>
          println("Некорректный ввод. Введите целое число.")
          loop
      }
    }

    loop
  }

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val program = for {
      seats <- readPositiveInt("Введите количество мест в барбершопе (не менее 2):")
      customers <- readPositiveInt("Введите количество клиентов:")

      _ = println(s"\nЗапуск симуляции: $seats мест, $customers клиентов...")

      simulationService = new SimulationService()
      stats <- simulationService.runSimulation(seats, customers)

      _ = {
        println("\n" + "="*50)
        println("РЕЗУЛЬТАТЫ СИМУЛЯЦИИ")
        println("="*50)
        println(s"Всего клиентов: ${stats.totalCustomers}")
        println(s"Обслужено: ${stats.servedCustomers}")
        println(s"Отказано: ${stats.rejectedCustomers}")
        println(f"Среднее время ожидания: ${stats.averageWaitTime / 1000}%.2f сек")
        println("\nСтатистика кэш-системы:")
        println(s"  L1 кэш: ${stats.cacheStats.l1.size} элементов")
        println(s"  L2 кэш: ${stats.cacheStats.l2.size} элементов")
        println(s"  L3 кэш: ${stats.cacheStats.l3.size} элементов")
        println(s"  Всего операций: ${stats.cacheStats.totalOperations}")
        println("="*50)
      }

    } yield ()

    program.onComplete {
      case Success(_) =>
        println("\nСимуляция успешно завершена!")
        System.exit(0)

      case Failure(e: TimeoutException) =>
        println(s"\nОшибка: Время выполнения симуляции истекло.")
        System.exit(1)

      case Failure(e) =>
        println(s"\nОшибка при выполнении симуляции: ${e.getMessage}")
        e.printStackTrace()
        System.exit(1)
    }

    try {
      Await.result(program, Duration.Inf)
    } catch {
      case _: InterruptedException =>
        println("\nПрограмма прервана.")
        System.exit(2)
    }
  }
}