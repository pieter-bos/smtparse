package smtparse

import smtparse.Verifier.DetachedVerification
import smtparse.output.{Dialect, SmtOutput}
import smtparse.parse.{Assert, Attribute, CheckSat, DeclareConst, DefineConst, SExpr, SetOption, SmtCommand}

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, StringWriter}
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
        val backupWriter = new StringWriter()
        val out = SmtOutput(writer, dialect)
        val backupOut = SmtOutput(backupWriter, dialect)
        def localWrite(c: SmtCommand) = {
          out.write(c)
          backupOut.write(c)
        }
        for((key, value) <- pinnedOptions) {
          localWrite(SetOption(Attribute(key, value)))
        }
        declarations.map {
          case d: DeclareConst => dialect.declareConst(d)
          case d: DefineConst => dialect.defineConst(d)
          case d => d
        }.foreach(localWrite)
        assertions.foreach(localWrite)
        localWrite(CheckSat)
        writer.close()

        val result = new BufferedReader(new InputStreamReader(p.getInputStream, StandardCharsets.UTF_8)).readLine() match {
          case "sat" => Sat
          case "unsat" => Unsat
          case "unknown" => Unknown
          case err =>
            val x = err
            println(s"---- Backup smt ----\n${backupWriter.toString}\n---- End backup smt ----")
            Error(x)
        }

        p.destroyForcibly()

        result
      }
    })
  }
}