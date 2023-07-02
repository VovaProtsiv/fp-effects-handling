package exercise

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import scala.concurrent.duration.DurationInt

object MemoizationExercise extends IOApp {
  trait Currency

  case object Dollar extends Currency

  case object Euro extends Currency

  case class Balance(amount: Double, currency: Currency)

  //pretend this is calling an API and take some time
  def fetchDollarExchangeRate(currency: Currency): Double = {
    Thread.sleep(2000)
    currency match {
      case Dollar => 1.0
      case Euro => 1.12
    }
  }

  def fetchDollarExchangeRateIO(currency: Currency): IO[Double] = {
    IO.sleep(3.seconds) *>
      IO.println("Fetching exchange rate...") *>
      IO.pure {
        currency match {
          case Dollar => 1.0
          case Euro => 1.12
        }
      }
  }

  lazy val euroExchangeRate: Double = fetchDollarExchangeRate(Euro)

  lazy val euroExchangeRateIO: IO[Double] = fetchDollarExchangeRateIO(Euro)


  def getBalanceInDollars(balances: List[Balance]): List[Double] = {
    balances.map(balance => balance.currency match {
      case Dollar => balance.amount
      case Euro => balance.amount * euroExchangeRate
    })
  }

  def getBalanceInDollarsIO(balances: List[Balance]): IO[List[Double]] = {
    euroExchangeRateIO.memoize.flatMap { exchangeRateIO =>
      balances.traverse {
        case Balance(amount, _@Euro) => exchangeRateIO.map(exchangeRate => amount * exchangeRate)
        case Balance(amount, _@Dollar) => IO.pure(amount)
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val balances1 = List(Balance(10.0, Dollar), Balance(1.0, Dollar), Balance(150.0, Dollar))
    val balances2 = List(Balance(20.0, Euro), Balance(2.0, Dollar), Balance(250.0, Euro))
    // Modify both functions so they return an IO
    // Achieve the same behaviour:
    // - If all balances are dollars, you never fetch the exchange rate
    // - If more than one balance is euros, you only fetch the exchange rate once
    getBalanceInDollarsIO(balances1).flatMap(IO.println) *>
      IO.pure(ExitCode.Success)
  }
}
