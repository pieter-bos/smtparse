package smtparse

import smtparse.parse.{Assert, CheckSat, DeclareConst, DeclareDataTypes, DeclareFun, DeclareSort, DefineConst, DefineFun, DefineSort, GetInfo, Pop, Push, SExpr, SetOption, SmtCommand, Symbol}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.language.postfixOps
import scala.util.{Failure, Success}

class Multiplexer(backends: Seq[Verifier]) {
  val options: mutable.Map[String, Option[SExpr]] = mutable.Map()
  val decls: ArrayBuffer[SmtCommand] = ArrayBuffer()
  var assns: Seq[State] = Seq(State())

  case class State(assertions: ArrayBuffer[Assert] = ArrayBuffer()) {
    def copy(): State = State(assertions.clone())
  }

  def successOrNothing: String =
    options.get("print-success") match {
      case Some(Some(Symbol("true"))) => "success\n"
      case _ => ""
    }

  def verify(): Verifier.Result = {
    val verifications = backends.map(_.startVerification(options.toMap, decls.toSeq, assns.last.assertions.toSeq))
    val firstComplete = Promise[Verifier.Result]()
    val unknownCount: AtomicInteger = new AtomicInteger(0)

    verifications.foreach(_.result.onComplete {
      case Failure(exception) => firstComplete.tryFailure(exception)
      case Success(value) => value match {
        case Verifier.Unsat => firstComplete.trySuccess(Verifier.Unsat)
        case Verifier.Sat => firstComplete.trySuccess(Verifier.Sat)
        case err: Verifier.Error => firstComplete.trySuccess(err)
        case Verifier.Unknown =>
          if(unknownCount.incrementAndGet() == verifications.size) {
            firstComplete.trySuccess(Verifier.Unknown)
          }
      }
    })

    val result = Await.result(firstComplete.future, 1001 days)
    verifications.foreach(_.abort())
    result
  }

  def addCommand(command: SmtCommand): String = command match {
    case GetInfo(key) => key match {
      case "version" => "(:version \"0.1-alpha1\")\n"
      case "all-statistics" => "()\n"
      case _ => "unsupported\n"
    }
    case SetOption(attr) =>
      options(attr.key) = attr.value
      successOrNothing
    case d: DeclareDataTypes => decls += d; successOrNothing
    case d: DeclareSort => decls += d; successOrNothing
    case d: DefineSort => decls += d; successOrNothing
    case d: DeclareConst => decls += d; successOrNothing
    case d: DefineConst => decls += d; successOrNothing
    case d: DeclareFun => decls += d; successOrNothing
    case d: DefineFun => decls += d; successOrNothing
    case a: Assert => assns.last.assertions += a; successOrNothing
    case CheckSat =>
      verify() match {
        case Verifier.Unsat => "unsat\n"
        case Verifier.Sat => "sat\n"
        case Verifier.Unknown => "unknown\n"
        case Verifier.Error(err) => println(err); ???
      }
    case Push(n) =>
      for(_ <- 0 until n.toInt) {
        assns = assns :+ assns.last.copy()
      }
      successOrNothing
    case Pop(n) =>
      assns = assns.dropRight(n.toInt)
      successOrNothing
  }
}
