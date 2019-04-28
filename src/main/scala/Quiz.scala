import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * 1 - read a line
  * 2 - print the answer to user
  * 3 - read user answer
  * 4 - check response
  */

object Quiz extends App {

  type QuizResult = Int
  type TotalQuestions = Int

  def readQuizFile(fileName: String): Try[List[String]] =
    Try(Source.fromResource(fileName).getLines.toList)

  def isFormatValid(content: List[String]): Boolean =
    content.foldLeft(true)((valid, line) => line.split(",").length == 2 && valid)

  def performQuiz(content: List[String]): (QuizResult, TotalQuestions) = {
    val correctAnswers = content.foldLeft(0)((correctAnswers, c) => {
      val parsed: Array[String] = c.split(",")
      val question: String = parsed(0)
      val correctAnswer: String = parsed(1)
      val answer: String = scala.io.StdIn.readLine(s"$question = ")
      answer == correctAnswer match {
        case true => correctAnswers + 1
        case false => correctAnswers
      }
    })

    (correctAnswers,content.length)
  }

  val quizLines: Try[List[String]] = readQuizFile("quiz.csv")
  quizLines match {
    case Success(lines) =>
      isFormatValid(lines) match {
        case true => println(performQuiz(lines))
        case false => print("Invalid file format :(")
      }
    case Failure(_) => println("No file found :(")
  }
}
