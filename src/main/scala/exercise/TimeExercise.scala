package exercise

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.time.{Instant, LocalDateTime, ZoneId}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object TimeExercise extends IOApp {
  def tomorrow(): IO[FiniteDuration] = {
    IO.realTime.map(_ + 1.day)
  }

  def tomorrowDateTime(): IO[LocalDateTime] = tomorrow().map { duration =>
    Instant
      .ofEpochMilli(duration.toMillis)
      .atZone(ZoneId.systemDefault())
      .toLocalDateTime
  }

  override def run(args: List[String]): IO[ExitCode] = {
    tomorrowDateTime()
      .flatMap(IO.println)
      .as(ExitCode.Success)
  }
}