package exercise

import cats.effect._
import cats.effect.implicits._
import cats.implicits._

object DeferredExercise extends IOApp {
  class Producer[A](name: String, deferred: Deferred[IO, A], exec: IO[A]) {
    def run(): IO[Unit] = exec.flatMap(a=>deferred.complete(a).as(()))
  }

  class Consumer[A](name: String, deferred: Deferred[IO, A], consume: A => IO[Unit]) {
    def run(): IO[Unit] ={
      deferred.get.flatMap(consume(_))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    IO.pure(ExitCode.Success)
  }
}
