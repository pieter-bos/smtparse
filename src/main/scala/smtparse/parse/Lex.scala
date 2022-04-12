package smtparse.parse

import smtparse.parse.Lex.LexError

import java.io.{IOException, Reader}
import scala.annotation.tailrec

case object Lex {
  case class LexError(err: String) extends IOException {
    override def getMessage: String = err
  }
}

case class Lex(in: Reader) extends Iterator[SmtToken] {
  private var lookahead: Option[Char] = None

  private def peek: Option[Char] = lookahead match {
    case Some(value) => Some(value)
    case None =>
      in.read() match {
        case -1 => None
        case other =>
          lookahead = Some(other.asInstanceOf[Char])
          lookahead
      }
  }

  private def read(): Option[Char] = lookahead match {
    case Some(value) =>
      lookahead = None
      Some(value)
    case None =>
      in.read() match {
        case -1 => None
        case other => Some(other.asInstanceOf[Char])
      }
  }

  @tailrec
  private def skipWhitespace(): Unit =
    peek match {
      case None =>
      case Some(' ') | Some('\t') | Some('\r') | Some('\n') => read(); skipWhitespace()
      case Some(';') =>
        read()
        while(peek.nonEmpty && peek.get != '\n' && peek.get != '\r') read()
        skipWhitespace()
      case Some(_) =>
    }

  override def hasNext: Boolean = {
    skipWhitespace()
    peek.nonEmpty
  }

  override def next(): SmtToken = {
    skipWhitespace()
    // hasNext is a precondition of next, so peek.nonEmpty
    (read().get match {
      case '(' => TokParenOpen
      case ')' => TokParenClose
      case '0' => TokNumeral(0)
      case c if "123456789".contains(c) =>
        val buf = new StringBuilder
        buf.append(c)
        while(peek.nonEmpty && "0123456789".contains(peek.get)) {
          buf.append(read().get)
        }
        TokNumeral(BigInt(buf.toString(), radix = 10))
      case '#' =>
        peek match {
          case None => throw LexError("Unterminated hex/binary constant")
          case Some('x') =>
            val buf = new StringBuilder
            while(peek.nonEmpty && "0123456789abcdefABCDEF".contains(peek.get)) {
              buf.append(read().get)
            }

            buf.toString() match {
              case "" => throw LexError("Empty hex constant")
              case other => TokHexadecimal(other)
            }
          case Some('b') =>
            val buf = new StringBuilder
            while(peek.nonEmpty && "01".contains(peek.get)) {
              buf.append(read().get)
            }

            buf.toString() match {
              case "" => throw LexError("Empty binary constant")
              case other => TokHexadecimal(other)
            }
          case Some(other) => throw LexError(s"Invalid number designator #$other")
        }
      case '"' =>
        val buf = new StringBuilder

        while(peek.nonEmpty && peek.get != '"') {
          buf.append(read().get)
        }

        peek match {
          case None => throw LexError("Unterminated string")
          case Some('"') => read()
          case _ => ???
        }

        TokStringLiteral(buf.toString())
      case c if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || "~!@$%^&*_-+=<>.?/".contains(c) =>
        val buf = new StringBuilder
        buf.append(c)

        while(peek.nonEmpty && ('a' <= peek.get && peek.get <= 'z') || ('A' <= peek.get && peek.get <= 'Z') || "~!@$%^&*_-+=<>.?/".contains(peek.get) || ('0' <= peek.get && peek.get <= '9')) {
          buf.append(read().get)
        }

        TokSymbol(buf.toString())
      case '|' =>
        val buf = new StringBuilder

        while(peek.nonEmpty && peek.get != '|') {
          if(peek.get == '\\') throw LexError("quoted symbol must not contain backslash")
          buf.append(read().get)
        }

        peek match {
          case None => throw LexError("Unterminated quoted symbol")
          case Some('|') => read()
          case _ => ???
        }

        TokSymbol(buf.toString())
      case ':' =>
        if(hasNext) {
          next() match {
            case TokSymbol(name) => TokKeyword(name)
            case other => throw LexError(s"colon must be immediately followed by symbol, but instead $other was encountered")
          }
        } else {
          throw LexError("Empty keyword")
        }
      case other => throw LexError(s"Invalid character: $other")
    }) match {
      case TokSymbol("!") => TokExclamationPoint
      case TokSymbol("_") => TokUnderscore
      case TokSymbol("as") => TokAs
      case TokSymbol("BINARY") => TokKwdBinary
      case TokSymbol("DECIMAL") => TokKwdDecimal
      case TokSymbol("exists") => TokExists
      case TokSymbol("HEXADECIMAL") => TokKwdHexadecimal
      case TokSymbol("forall") => TokForall
      case TokSymbol("let") => TokLet
      case TokSymbol("match") => TokMatch
      case TokSymbol("NUMERAL") => TokKwdNumeral
      case TokSymbol("par") => TokPar
      case TokSymbol("STRING") => TokKwdString

      case TokNumeral(n) =>
        peek match {
          case Some('.') =>
            read()
            var value = n
            var scale = 0
            while(peek.nonEmpty && '0' <= peek.get && peek.get <= '9') {
              value *= 10
              value += "0123456789".indexOf(read().get)
              scale += 1
            }
            TokDecimal(BigDecimal(value, scale))
          case _ => TokNumeral(n)
        }

      case other => other
    }
  }
}
