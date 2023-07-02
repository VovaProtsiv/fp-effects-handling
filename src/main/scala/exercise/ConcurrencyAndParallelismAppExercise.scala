package exercise

import cats.effect._
import cats.implicits._

import scala.concurrent.duration.DurationInt

object ConcurrencyAndParallelismAppExercise extends IOApp {
  case class Quote(author: String, text: String)

  def fetchHttp(n: Int): IO[List[Quote]] =
    IO.sleep(10.millis) *>
      (1 to n).toList.map(i => Quote(s"author $i", s"text $i")).pure[IO]

  def fetchDb(n: Int): IO[List[Quote]] =
    IO.sleep(100.millis) *>
      (1 to n).toList.map(i => Quote(s"author $i", s"text $i")).pure[IO]

  def fetchAuthorAge(author: String): IO[Int] =
    IO.sleep(150.millis) *> IO((math.random() * 100).toInt)


  override def run(args: List[String]): IO[ExitCode] = {
    val n = 3

    // fetch n quotes from the fastest source
    // calculate the average age of the authors
    val authors:IO[List[String]] = IO.race(fetchHttp(n), fetchDb(n)).map {
      case Left(httpQuotes) => httpQuotes
      case Right(dbQuotes) => dbQuotes
    }.map(ls=>ls.map(quote=>quote.author))
    val averageAge = authors.flatMap(ls => ls.parTraverse(author => fetchAuthorAge(author)))
      .flatTap(IO.println)
      .map(ls => ls.sum / ls.size)
    averageAge.flatTap(IO.println).as(ExitCode.Success)
  }
}
