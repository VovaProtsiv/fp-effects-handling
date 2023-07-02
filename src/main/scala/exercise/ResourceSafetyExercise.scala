package exercise

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.io._
import java.net.{HttpURLConnection, URL}

object ResourceSafetyExercise extends IOApp {
  def createConnection(targetURL: String): IO[HttpURLConnection] = {
    IO.println("Create connections") *>
    IO.blocking {
      val connection = new URL(targetURL).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }
  }

  def readOutput(reader: BufferedReader): IO[String] = {
    IO.println("Reading ...") *>
    IO.blocking {
      Iterator
        .continually(reader.readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    }
  }


  def httpGet(targetURL: String): IO[String] = {
    for {
      connection <- createConnection(targetURL)
      inputStream <- IO(connection.getInputStream)
      reader <- IO(new BufferedReader(new InputStreamReader(inputStream))) // reader must be closed
      response <- readOutput(reader)
    } yield response
  }

  private def disconnectHttpURLConnection(connection: HttpURLConnection): IO[Unit] = {
    IO.println("Disconnect connection") *> IO.blocking(connection.disconnect())
  }

  def makeHttpURLConnectionResource(targetURL: String): Resource[IO, HttpURLConnection] = {
    Resource.make(createConnection(targetURL))(connection => disconnectHttpURLConnection(connection))
  }

  def makeReaderResource(connection: HttpURLConnection): Resource[IO, BufferedReader] = {
    Resource.fromAutoCloseable {
      for {
        inputStream <- IO(connection.getInputStream)
        reader <- IO(new BufferedReader(new InputStreamReader(inputStream)))
      } yield reader
    }
  }

  def httpGet2(targetURL: String): IO[String] = {
   val connectionReaderRes = for{
     connection <- makeHttpURLConnectionResource(targetURL)
     reader <- makeReaderResource(connection)
   } yield (connection,reader)

    connectionReaderRes.use{
      case (_, reader) => readOutput(reader)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    httpGet2("http://www.google.com")
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}

