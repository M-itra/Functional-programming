import scala.util.Try
import scala.util.Success 
import scala.util.Failure

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import scala.io.StdIn.readLine


// 1
def myFunction(x: Double): Double = x * x

def integrate(f:Double =>Double, l:Double, r:Double, steps:Int):Double ={
    val h = (r - l) / steps
    (0 until steps).map(i => f(l + i * h) * h).sum
}

@main def task()={
    val result = integrate(myFunction, 0.0, 3.0, 10000)
    println(result)
}


// 2
// 2.1
def goodEnoughPassword(password:String): Option[Boolean]={
    val conditions: List[Boolean] = List( 
        password.length >= 8,
        password.exists(_.isUpper),
        password.exists(_.isLower),
        password.exists(_.isDigit),
        password.exists(ch => !ch.isLetterOrDigit),
    )

    if (conditions.forall(identity)) Some(true) else None
}

// 2.2
def goodEnoughPasswordTry(password: String): Either[String, String] = {
  Try {
    val errorMessage: Seq[String] = Seq(
      (password.length >= 8, "Строка длиной меньше 8"),
      (password.exists(_.isUpper), "Отсутствует заглавный символ"),
      (password.exists(_.isLower), "Отсутствует строчный символ"),
      (password.exists(_.isDigit), "Отсутствует цифра"),
      (password.exists(ch => !ch.isLetterOrDigit), "Отсутствует специальный символ")
      ).collect { case (false, message) => message }

    if (errorMessage.isEmpty) Left("Пароль подходит") 
    else Right(errorMessage.mkString(",\n"))
    
  } match {
      case Success(result) => result
      case Failure(exception) => Right(s"Ошибка: ${exception.getMessage}")
  }
}

// 2.3
def readPassword(): Future[String] = {
  Future {
    def loop(): String = {
      print("Введите пароль: ")
      val password = readLine()

      goodEnoughPasswordTry(password) match {
        case Left(_) =>
          password + "\n"
        case Right(errors) =>
          println(s"Ошибки:\n$errors")
          loop()
      }
    }
    loop()
  }
}

@main def task2()={
    // 2.1
    println(goodEnoughPassword("123Aa"))
    println(goodEnoughPassword("123Aa1234"))
    println(goodEnoughPassword("12346Ф;d"))

    // 2.2
    println(goodEnoughPasswordTry("123Aa"))
    println(goodEnoughPasswordTry("123Aa1234"))
    println(goodEnoughPasswordTry("12346Ф;d"))

    // 2.3
    print(Await.result(readPassword(), Duration.Inf))
}


// 3
// 3.1
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

// 3.2
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}