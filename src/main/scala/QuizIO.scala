import cats.Semigroupal
import cats.effect.IO

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * 1 - read a line
  * 2 - print the answer to user
  * 3 - read user answer
  * 4 - check response
  */

object QuizIO extends App {

  type QuizResult = Int
  type TotalQuestions = Int

  def readLn(prompt: String): IO[String] = IO(scala.io.StdIn.readLine(prompt))
  def putStrLn(value: String): IO[Unit] = IO(println(value))
  def readFromSource(filename: String): IO[Try[List[String]]] = IO(Try(Source.fromResource(filename).getLines.toList))

  def isFormatValid(content: List[String]): Boolean =
    content.foldLeft(true)((valid, line) => line.split(",").length == 2 && valid)

  def performQuiz(content: List[String]): IO[Unit] = {
    val correctAnswers: IO[TotalQuestions] = content.foldLeft(IO(0))((caIO, c) => {
      val parsed: Array[String] = c.split(",")
      val question: String = parsed(0)
      val correctAnswer: String = parsed(1)
      val answerIO: IO[String] = readLn(s"$question = ")

      for {
        correctAndUserInput <- Semigroupal[IO].product(caIO, answerIO)
        counter <- correctAndUserInput._2 == correctAnswer match {
          case true => IO(correctAndUserInput._1 + 1)
          case false => IO(correctAndUserInput._1)
        }
      } yield counter
    })

    correctAnswers.flatMap { ca => putStrLn(s"$ca correct answers of ${content.length} total") }
  }

  val p = for {
    quizQuestions <- readFromSource("quiz.csv")
    _ <- quizQuestions match {
      case Success(lines) =>
        isFormatValid(lines) match {
          case true => performQuiz(lines)
          case false => putStrLn("Invalid file format :(")
        }
      case Failure(_) => putStrLn("No file found :(")
    }
  } yield ()

  p.unsafeRunSync()

}
