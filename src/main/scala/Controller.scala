import Controller.{Request, postTransfer}
import cats.effect._
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._

import scala.util.Success

object Controller {

  case class Request(fromAccount: String, toAccount: String, amount: String)

  case class Response(status: Int, body: String)


  def postTransfer(request: Request): IO[Response] = {

    val validatedInput = (Validations.validateAccountNumber(request.fromAccount),
      Validations.validateAccountNumber(request.toAccount),
      Validations.validateDouble(request.amount)).tupled

    val response = validatedInput match {
      case Validated.Valid((from, to, amount)) => {
        Service.transfer(from, to, amount).map {
          case Left(error: DomainError) => Response(400, error.toString)
          case Right(_) => Response(200, "The transfer successfully executed")
        }
      }
      case Validated.Invalid(e) => IO(Response(400, e.mkString_(", ")))
    }
    response.handleErrorWith(_ => IO(Response(500, "Internal server error")))
  }
}

object ErrorHandlingApp extends IOApp {
  import Models._
  override def run(args: List[String]): IO[ExitCode] = {
    val request = Request("1234", "3456", "2000")
    for {
      _ <- Repository.saveAccount(Account("1234",10000))
      _ <- Repository.saveAccount(Account("3456",4000))
      transfer <- postTransfer(request)
      _ <- IO.println(transfer)
      _ <- IO.println(Repository.data)
    } yield (ExitCode.Success)

    /*postTransfer(request)
          .flatTap(IO.println)
          .as(ExitCode.Success)*/

  }
}

object Validations {
  type Valid[A] = ValidatedNec[String, A]

  def validateDouble(s: String): Valid[Double] = {
    Validated.fromOption(s.toDoubleOption, NonEmptyChain.one(
      s"$s is not a valid double"))
  }

  def validateAccountNumber(accountNumber: String): Valid[String] = {
    Validated.condNec(
      accountNumber.forall(_.isLetterOrDigit)
      , accountNumber
      , s"$accountNumber must contains only letters and digits.")
  }
}


trait DomainError

case class InsufficientBalanceError(actualBalance: Double, amountToWithdraw: Double) extends DomainError

case class MaximumBalanceExceededError(actualBalance: Double, amountToDeposit: Double) extends DomainError

case class AccountNotFoundError(accountNumber: String) extends DomainError

object Models {
  val maxBalance = 5000.0

  case class Account(number: String, balance: Double) {
    def withdraw(amount: Double): Either[DomainError, Account] = {
      Either.cond(
        balance >= amount,
        this.copy(balance = balance - amount),
        InsufficientBalanceError(balance, amount))
    }

    def deposit(amount: Double): Either[DomainError, Account] = {
      val bal = balance + amount
      Either.cond(
        bal < maxBalance,
        this.copy(balance = balance + amount),
        MaximumBalanceExceededError(balance, amount)
      )
    }
  }
}

object Repository {

  import Models._

  var data = Map.empty[String, Account]

  //def findAccountByNumber(number: String): IO[Option[Account]] = IO.pure(data.get(number))
  def findAccountByNumber(number: String): IO[Option[Account]] = IO.raiseError(new RuntimeException("bada-boom"))

  def saveAccount(account: Account): IO[Unit] = IO{
    data = data + (account.number -> account)}
}

object Service {

  import Models._

  def transfer(fromAccountNumber: String, toAccountNumber: String, amount: Double): IO[Either[DomainError, Unit]] = {
    Repository.findAccountByNumber(fromAccountNumber).flatMap { fromAccountNumberOpt =>
      Repository.findAccountByNumber(toAccountNumber).flatMap { toAccountNumberOpt =>
        val accounts: Either[DomainError, (Account, Account)] = for {
          fromAccount <- fromAccountNumberOpt.toRight(AccountNotFoundError(fromAccountNumber))
          toAccount <- toAccountNumberOpt.toRight(AccountNotFoundError(toAccountNumber))
          withdrawAcc <- fromAccount.withdraw(amount)
          depositAcc <- toAccount.deposit(amount)
        } yield (withdrawAcc, depositAcc)
        accounts.traverse {
          case (from, to) => Repository.saveAccount(from) *> Repository.saveAccount(to)
        }
      }
    }
  }
}




























