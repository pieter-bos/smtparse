package smtsolve.smt

import smtsolve.smt.Parse.ParseError

import java.io.Reader

case object Parse {
  case class ParseError(err: String) extends Exception {
    override def getMessage: String = err
  }
}

case class Parse(reader: Reader) extends Iterator[SmtCommand] {
  private val in = Lex(reader)

  private var lookahead: Option[SmtToken] = None

  override def hasNext: Boolean = peek.nonEmpty

  private def peek: Option[SmtToken] = lookahead match {
    case Some(token) => Some(token)
    case None =>
      if(!in.hasNext) None
      else {
        lookahead = Some(in.next())
        lookahead
      }
  }

  private def consume(): SmtToken = lookahead match {
    case Some(token) =>
      lookahead = None
      token
    case None =>
      in.next()
  }

  private def expectAny(): SmtToken =
    peek match {
      case Some(token) => consume(); token
      case None => throw ParseError("Unexpected end of token stream.")
    }

  private def expect(token: SmtToken): Unit =
    peek match {
      case Some(`token`) => consume()
      case Some(other) => throw ParseError(s"Expected $token, but got $other")
      case None => throw ParseError("Unexpected end of token stream.")
    }

  private def expectKeyword(): Keyword =
    peek match {
      case Some(k: Keyword) => consume(); k
      case Some(other) => throw ParseError(s"Expected keyword, but got $other")
      case None => throw ParseError("Unexpected end of token stream.")
    }

  private def expectSymbol(): Symbol =
    peek match {
      case Some(s: Symbol) => consume(); s
      case Some(other) => throw ParseError(s"Expected symbol, but got $other")
      case None => throw ParseError("Unexpected end of token stream.")
    }

  override def next(): SmtCommand = {
    expect(ParenOpen)
    val result = expectSymbol() match {
      case Symbol("get-info") => GetInfo(expectKeyword().key)
      case Symbol("set-option") => SetOption(expectKeyword().key, )
      case Symbol(other) => throw ParseError(s"Unknown command $other")
    }
    expect(ParenClose)
    result
  }
}
