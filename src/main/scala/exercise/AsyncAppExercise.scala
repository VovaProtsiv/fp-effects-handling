package exercise

import cats.effect._
import cats.implicits._

import scala.concurrent.Promise

object AsyncAppExercise extends IOApp {
  case class User(id: Long, username: String)
  type Error = String

  def findUser(id: Long)(cb: Either[Error, User] => Unit): Unit = {
    if(math.random() < 0.5) cb(Right(User(id, s"User $id")))
    else                    cb(Left("Something went wrong"))
  }

  def findUserIO(id: Long): IO[User] = {
    IO.async_{ cb =>
      findUser(id) {
        case Left(error) => cb(Left(new Throwable(error)))
        case Right(user) => cb(Right(user))
      }

    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    findUserIO(5).handleErrorWith(_=>IO.println("Something went wrong"))
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
