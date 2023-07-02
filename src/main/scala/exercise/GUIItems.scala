package exercise

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import scala.concurrent.duration.DurationInt

object GUIItems extends IOApp {

  case class Item(id: Int)

  val items = List(Item(1), Item(2), Item(3))

  def loadItems(): IO[List[Item]] = IO.sleep(2.seconds) *>IO.println("Items are loading")*> IO.pure(items)

  def initUI(): IO[Unit] = IO.sleep(2.seconds) *> IO.println("IO was initialized")

  def showItems(items: List[Item]): IO[Unit] = items.parTraverse_(item=>IO.println(item))

  def showError(): IO[Unit] = IO.println("Ooops! Something went wrong!")

  def setupUI(): IO[Unit] = {
    (initUI(), loadItems().attempt).parMapN{
      case (_,Right(items))=> showItems(items)
      case (_,Left(_))=> showError() }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    setupUI().as(ExitCode.Success)
  }
}
