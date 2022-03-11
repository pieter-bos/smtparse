package smtsolve.smt

import smtsolve.smt.Lex.LexError

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
    read().get match {
      case '(' => ParenOpen
      case ')' => ParenClose
      case '0' => Numeral(0)
      case c if "123456789".contains(c) =>
        val buf = new StringBuilder
        buf.append(c)
        while(peek.nonEmpty && "0123456789".contains(peek.get)) {
          buf.append(read().get)
        }
        Numeral(BigInt(buf.toString(), radix = 10))
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
              case other => Hexadecimal(other)
            }
          case Some('b') =>
            val buf = new StringBuilder
            while(peek.nonEmpty && "01".contains(peek.get)) {
              buf.append(read().get)
            }

            buf.toString() match {
              case "" => throw LexError("Empty binary constant")
              case other => Hexadecimal(other)
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

        StringLiteral(buf.toString())
      case c if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || "~!@$%^&*_-+=<>.?/".contains(c) =>
        val buf = new StringBuilder
        buf.append(c)

        while(peek.nonEmpty && ('a' <= peek.get && peek.get <= 'z') || ('A' <= peek.get && peek.get <= 'Z') || "~!@$%^&*_-+=<>.?/".contains(peek.get) || ('0' <= peek.get && peek.get <= '9')) {
          buf.append(read().get)
        }

        Symbol(buf.toString())
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

        Symbol(buf.toString())
      case ':' =>
        if(hasNext) {
          next() match {
            case Symbol(name) => Keyword(name)
            case other => throw LexError(s"colon must be immediately followed by symbol, but instead $other was encountered")
          }
        } else {
          throw LexError("Empty keyword")
        }
      case other => throw LexError(s"Invalid character: $other")
    }
  }
}
