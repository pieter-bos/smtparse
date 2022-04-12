package smtparse.output

import smtparse.parse
import smtparse.parse.{Apply, Assert, Attribute, Attributed, Bin, Binding, CheckSat, Const, Constructor, DataType, DeclareConst, DeclareDataTypes, DeclareFun, DeclareSort, DefineConst, DefineFun, DefineSort, Exists, Forall, GetInfo, Hex, Identifier, IndexedIdentifier, Keyword, Let, Numeral, Pop, Push, QualifiedIdentifier, Reserved, SExpr, SExprApply, SetOption, SimpleIdentifier, SimpleSort, SmtCommand, SmtDecimal, SmtReservedSymbol, SmtString, Sort, SortApply, SortDec, SortedIdentifier, Term, TokAs, TokExclamationPoint, TokExists, TokForall, TokKwdBinary, TokKwdDecimal, TokKwdHexadecimal, TokKwdNumeral, TokKwdString, TokLet, TokMatch, TokPar, TokUnderscore, UnqualifiedIdentifier, Var}

import java.io.{File, FileWriter, Writer}

case class SmtOutput(stream: Writer, dialect: Dialect) {
  private val debugOut = new FileWriter(new File("/tmp/output.smt2"))

  def write(text: String): this.type = { debugOut.write(text); debugOut.flush(); stream.write(text); this }

  def writeStringList(texts: Seq[String]): this.type =
    texts match {
      case Nil => write("()")
      case text :: texts =>
        write("(")
        write(text)
        for(text <- texts) {
          write(" ")
          write(text)
        }
        write(")")
    }

  def write(const: Const): this.type = const match {
    case Numeral(n) => write(n.toString(radix = 10))
    case SmtDecimal(n) => write(n.toString())
    case Hex(data) => ???
    case Bin(data) => ???
    case SmtString(data) =>
      write("\"")
      data.foreach {
        case '"' => write("\"\"")
        case other => write(other.toString)
      }
      write("\"")
  }

  def write(symbol: SmtReservedSymbol): this.type = symbol match {
    case TokExclamationPoint => write("!")
    case TokUnderscore => write("_")
    case TokAs => write("as")
    case TokKwdBinary => write("BINARY")
    case TokKwdDecimal => write("DECIMAL")
    case TokExists => write("exists")
    case TokKwdHexadecimal => write("HEXADECIMAL")
    case TokForall => write("forall")
    case TokLet => write("let")
    case TokMatch => write("match")
    case TokKwdNumeral => write("NUMERAL")
    case TokPar => write("par")
    case TokKwdString => write("STRING")
  }

  def write(symbol: parse.Symbol): this.type =
    if(!symbol.name.head.isDigit && symbol.name.forall { c =>
      c.isLetterOrDigit || "+-/*=%?!.$_~&^<>@".contains(c)
    }) {
      write(symbol.name)
    } else {
      write("|").write(symbol.name).write("|")
    }

  def write(expr: SExpr): this.type = expr match {
    case const: Const => write(const)
    case symbol: parse.Symbol => write(symbol)
    case Keyword(name) => write(":").write(name)
    case Reserved(symbol) => write(symbol)
    case SExprApply(args) => args match {
      case Nil => write("()")
      case arg :: args =>
        write("(")
        write(arg)
        for(arg <- args) write(" ").write(arg)
        write(")")
    }
  }

  def write(binding: Binding): this.type =
    write("(").write(binding.name).write(" ").write(binding.value).write(")")

  def write(term: Term): this.type = term match {
    case const: Const => write(const)
    case symbol: parse.Symbol => write(symbol)
    case Keyword(name) => write(":").write(name)
    case Apply(f, args) =>
      write("(")
      write(f)
      for(arg <- args) {
        write(" ")
        write(arg)
      }
      write(")")
    case identifier: QualifiedIdentifier => identifier match {
      case UnqualifiedIdentifier(id) => write(id)
      case SortedIdentifier(id, sort) => write("(as ").write(id).write(" ").write(sort).write(")")
    }
    case Let(bindings, body) =>
      write("(let ")
      bindings match {
        case Nil => write("()")
        case binding :: bindings =>
          write("(")
          write(binding)
          for(binding <- bindings) {
            write(" ")
            write(binding)
          }
          write(") ")
      }
      write(body)
      write(")")
    case Forall(vars, body) =>
      write("(forall ")
      writeVarList(vars)
      write(" ")
      write(body)
      write(")")
    case Exists(vars, body) =>
      write("(exists ")
      writeVarList(vars)
      write(" ")
      write(body)
      write(")")
    case Attributed(e, attrs) =>
      attrs match {
        case Nil => dialect match {
          case Dialect.Z3 => write("(! ").write(e).write(")")
          case Dialect.CVC5 => write(e)
        }
        case _ =>
          write("(! ")
          write(e)
          for(attr <- attrs) {
            write(" ")
            write(attr)
          }
          write(")")
      }
  }

  def write(id: Identifier): this.type = id match {
    case SimpleIdentifier(name) => write(dialect.symbol(name))
    case IndexedIdentifier(name, indices) => ???
  }

  def write(s: Sort): this.type = s match {
    case SimpleSort(name) => write(name)
    case SortApply(name, args) =>
      write("(")
      write(name)
      for(arg <- args) {
        write(" ")
        write(arg)
      }
      write(")")
  }

  def writeSortList(sorts: Seq[Sort]): this.type =
    sorts match {
      case Nil => write("()")
      case sort :: sorts =>
        write("(")
        write(sort)
        for(sort <- sorts) {
          write(" ")
          write(sort)
        }
        write(")")
    }

  def boxSingularPattern(pattern: SExpr): SExpr =
    pattern match {
      case SExprApply(args) =>
        // If any element of the pattern is just a symbol, likely the pattern is not wrapped in a list.
        if(args.forall(_.isInstanceOf[SExprApply])) SExprApply(args)
        else SExprApply(Seq(SExprApply(args)))
      case other => other // ???
    }

  def write(attr: Attribute): this.type = {
    write(":")
    write(attr.key)
    attr.value match {
      case Some(value) =>
        write(" ")
        attr.key match {
          case "pattern" => dialect match {
            case Dialect.Z3 => write(value)
            case Dialect.CVC5 => write(boxSingularPattern(value))
          }
          case _ => write(value)
        }
      case None => this
    }
  }

  def write(cons: Constructor): this.type = {
    write("(")
    write(cons.name)
    for((name, sort) <- cons.selectors) {
      write(" (")
      write(name)
      write(" ")
      write(sort)
      write(")")
    }
    write(")")
  }

  def writeDatatypes(types: Seq[(SortDec, DataType)]): this.type =
    dialect match {
      case Dialect.Z3 =>
        write("() (")
        for(((sort, t), idx) <- types.zipWithIndex) {
          if(idx != 0) write(" ")
          write("(")
          write(sort.name)
          for(cons <- t.constructors) {
            write(" ")
            write(cons)
          }
          write(")")
        }
        write(")")
      case Dialect.CVC5 =>
        write("(")
        for(((SortDec(name, n), _), idx) <- types.zipWithIndex) {
          if(idx != 0) write(" ")
          write("(")
          write(name)
          write(" ")
          write(n.toString(radix = 10))
          write(")")
        }
        write(") (")
        for(((_, DataType(_, conss)), idx) <- types.zipWithIndex) {
          if(idx != 0) write(" ")
          write("(")
          for((cons, idx) <- conss.zipWithIndex) {
            if(idx != 0) write(" ")
            write(cons)
          }
          write(")")
        }
        write(")")
    }

  def write(sort: SortDec): this.type =
    write(sort.name).write(" ").write(sort.n.toString(radix = 10))

  def write(v: Var): this.type =
    write("(").write(v.name).write(" ").write(v.t).write(")")

  def writeVarList(vars: Seq[Var]): this.type =
    vars match {
      case Nil => write("()")
      case v :: vars =>
        write("(")
        write(v)
        for(v <- vars) {
          write(" ")
          write(v)
        }
        write(")")
    }

  def write(command: SmtCommand): this.type = {
    command match {
      case GetInfo(key) => write("(get-info ").write(key).write(")")
      case SetOption(attr) => write("(set-option ").write(attr).write(")")
      case DeclareDataTypes(decls) => write("(declare-datatypes ").writeDatatypes(decls).write(")")
      case DeclareSort(decl) => write("(declare-sort ").write(decl).write(")")
      case DefineSort(name, args, defn) => write("(define-sort ").write(name).writeStringList(args).write(" ").write(defn).write(")")
      case DeclareConst(name, sort) => write("(declare-const ").write(name).write(" ").write(sort).write(")")
      case DefineConst(name, sort, value) => write("(define-const ").write(name).write(" ").write(sort).write(" ").write(value).write(")")
      case DeclareFun(name, args, res) => write("(declare-fun ").write(name).write(" ").writeSortList(args).write(" ").write(res).write(")")
      case DefineFun(name, args, res, impl) => write("(define-fun ").write(name).write(" ").writeVarList(args).write(" ").write(res).write(" ").write(impl).write(")")
      case Assert(t) => write("(assert ").write(t).write(")")
      case CheckSat => write("(check-sat)")
      case Push(n) => write("(push ").write(n.toString(radix = 10)).write(")")
      case Pop(n) => write("(pop ").write(n.toString(radix = 10)).write(")")
    }
    write("\n")
  }
}
