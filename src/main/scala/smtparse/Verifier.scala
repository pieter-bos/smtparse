package smtparse

import smtparse.Verifier.DetachedVerification
import smtparse.output.{Dialect, SmtOutput}
import smtparse.parse.{Assert, Attribute, CheckSat, SExpr, SetOption, SmtCommand}

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import scala.concurrent.{ExecutionContext, Future, blocking}

object Verifier {
  sealed trait Result
  case object Unsat extends Result
  case object Sat extends Result
  case object Unknown extends Result
  case class Error(firstLine: String) extends Result

  case class DetachedVerification(abort: () => Unit, result: Future[Result])
}

trait Verifier {
  def startVerification(options: Map[String, Option[SExpr]], declarations: Seq[SmtCommand], assertions: Seq[Assert]): DetachedVerification
}

case class PinnedOptionsVerifier(process: Seq[String], dialect: Dialect,
                                 pinnedOptions: Map[String, Option[SExpr]])
                                (implicit ec: ExecutionContext) extends Verifier {
  import Verifier._

  override def startVerification(options: Map[String, Option[SExpr]], declarations: Seq[SmtCommand], assertions: Seq[Assert]): DetachedVerification = {
    val builder = new ProcessBuilder()
    builder.command(process : _*)
    val p = builder.start()

    DetachedVerification(() => p.destroyForcibly(), Future {
      blocking {
        val writer = new OutputStreamWriter(p.getOutputStream, StandardCharsets.UTF_8)
        val out = SmtOutput(writer, dialect)
        for((key, value) <- pinnedOptions) {
          out.write(SetOption(Attribute(key, value)))
        }
        declarations.foreach(out.write)
        assertions.foreach(out.write)
        out.write(CheckSat)
        writer.close()

        val result = new BufferedReader(new InputStreamReader(p.getInputStream, StandardCharsets.UTF_8)).readLine() match {
          case "sat" => Sat
          case "unsat" => Unsat
          case "unknown" => Unknown
          case err => Error(err)
        }

        p.destroyForcibly()

        result
      }
    })
  }
}