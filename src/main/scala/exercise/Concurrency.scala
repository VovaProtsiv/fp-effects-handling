package exercise

import cats.effect.IO
import cats.implicits._

object Concurrency {
  case class Person(name: String)

  class PersonService {
    def createPerson(name: String): IO[Person] = IO(Person(name))

    def createAll(names: List[String]): IO[List[Person]] = {
      names.parTraverse(createPerson)
    }
  }

  case class Quote(author: String, text: String)

  class QuoteService {
    def kittensQuotes(n: Int): IO[List[Quote]] = ???

    def puppiesQuotes(n: Int): IO[List[Quote]] = ???

    def mixedQuotes(n: Int): IO[List[Quote]] = {
      (kittensQuotes(n), puppiesQuotes(n)).parMapN {
        (kittens,pappies)=>kittens++pappies
      }
    }
  }

  case class Image(data:List[Byte])

  class ImageService {
    def fetchFromDb(n:Int):IO[List[Image]] = ???
    def fetchFromHttp(n:Int):IO[List[Image]] = ???
    def fetchFromFastest(n:Int):IO[List[Image]] = {
      IO.race(fetchFromDb(n),fetchFromHttp(n))
        .map {
          case Left(value) => value
          case Right(value) => value
        }
    }
  }
}
