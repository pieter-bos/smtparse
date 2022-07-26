package smtparse.parse

import smtparse.parse.Parse.ParseError

import java.io.Reader
import scala.annotation.tailrec

case object Parse {
  case class ParseError(text: String) extends Exception {
    override def toString: String = text
  }
}

case class Parse(reader: Reader) extends Iterator[SmtCommand] {
  import Parse._

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

  var i = 0
  private def consume(): SmtToken = {
    val x = lookahead match {
      case Some(token) =>
        lookahead = None
        token
      case None =>
        in.next()
    }
//    i += 1
//    println(s"Consuming: $x ($i)")
    x
  }

  private def expect[T](f: PartialFunction[SmtToken, T]): T = {
    val tok = consume()
    f.lift(tok) match {
      case Some(value) => value
      case None =>
        println("Oops:")
        while (in.hasNext) {
          println(in.next())
        }
        throw ParseError(s"unexpected token $tok")
    }
  }

  private def select[T](f: PartialFunction[SmtToken, T]): T = {
    val tok = peek match {
      case Some(value) => value
      case None => throw ParseError("unexpected end of stream")
    }

    f.lift(tok) match {
      case Some(value) => value
      case None => throw ParseError(s"unexpected token $tok")
    }
  }

  private def selectMaybe[T](f: PartialFunction[SmtToken, T]): Option[T] =
    f.lift(peek match {
      case Some(value) => value
      case None => throw ParseError("unexpected end of stream")
    })

  def symbols(acc: Seq[Symbol] = Nil): Seq[Symbol] =
    selectMaybe {
      case s: TokSymbol => Symbol(s.name)
    } match {
      case Some(value) => symbols(acc :+ value)
      case None => acc
    }

  def const(): Const =
    expect {
      case TokNumeral(n) => Numeral(n)
      case TokDecimal(n) => SmtDecimal(n)
      case TokHexadecimal(n) => Hex(n)
      case TokBinary(data) => Bin(data)
      case TokStringLiteral(data) => SmtString(data)
    }

  def sExpr(): SExpr =
    select {
      case _: TokNumeral | _: TokDecimal | _: TokHexadecimal | _: TokBinary | _: TokStringLiteral => const()
      case TokSymbol(s) => expect { case TokSymbol(s) => Symbol(s) }
      case reserved: SmtReservedSymbol => expect { case reserved: SmtReservedSymbol => Reserved(reserved) }
      case TokKeyword(k) => expect { case TokKeyword(k) => Keyword(k) }
      case TokParenOpen =>
        expect { case TokParenOpen => () }
        val result = SExprApply(starSExpr())
        expect { case TokParenClose => () }
        result
    }

  def maybeSExpr(): Option[SExpr] =
    selectMaybe {
      case _: TokNumeral | _: TokDecimal | _: TokHexadecimal | _: TokBinary | _: TokStringLiteral |
           _: TokSymbol | _: SmtReservedSymbol | _: TokKeyword | TokParenOpen =>
        sExpr()
    }

  @tailrec
  private def starSExpr(acc: Seq[SExpr] = Nil): Seq[SExpr] =
    maybeSExpr() match {
      case Some(value) => starSExpr(acc :+ value)
      case None => acc
    }

  def index(): Index =
    expect {
      case TokNumeral(n) => Numeral(n)
      case TokSymbol(s) => Symbol(s)
    }

  def maybeIndex(): Option[Index] =
    selectMaybe {
      case _: TokNumeral | _: TokSymbol => index()
    }

  @tailrec
  private def starIndex(acc: Seq[Index] = Nil): Seq[Index] =
    maybeIndex() match {
      case Some(value) => starIndex(acc :+ value)
      case None => acc
    }

  def indexedIdentifierContinuation(): Identifier =
    expect {
      case TokUnderscore =>
        val name = expect { case TokSymbol(s) => s }
        val result = IndexedIdentifier(name, index() +: starIndex())
        expect { case TokParenClose => () }
        result
    }

  def identifier(): Identifier =
    expect {
      case TokSymbol(s) => SimpleIdentifier(s)
      case TokParenOpen => indexedIdentifierContinuation()
    }

  def qualifiedIdentifier(): QualifiedIdentifier =
    expect {
      case TokSymbol(s) => UnqualifiedIdentifier(SimpleIdentifier(s))
      case TokParenOpen =>
        select {
          case TokAs =>
            consume()
            val result = SortedIdentifier(identifier(), sort())
            expect { case TokParenClose => () }
            result
          case TokUnderscore =>
            UnqualifiedIdentifier(indexedIdentifierContinuation())
        }
    }

  def sort(): Sort =
    expect {
      case TokSymbol(s) => SimpleSort(SimpleIdentifier(s))
      case TokParenOpen =>
        select {
          case TokUnderscore => SimpleSort(indexedIdentifierContinuation())
          case _ =>
            val name = identifier()
            val args = sort() +: starSort()
            SortApply(name, args)
        }
    }

  def starSort(acc: Seq[Sort] = Nil): Seq[Sort] =
    select {
      case _: TokSymbol | TokParenOpen => starSort(acc :+ sort())
      case _ => acc
    }

  def attribute(): Attribute = {
    val key = expect { case TokKeyword(k) => k }
    Attribute(key, maybeSExpr())
  }

  def maybeAttribute(): Option[Attribute] =
    selectMaybe {
      case _: TokKeyword => attribute()
    }

  @tailrec
  private def starAttribute(acc: Seq[Attribute] = Nil): Seq[Attribute] =
    maybeAttribute() match {
      case Some(value) => starAttribute(acc :+ value)
      case None => acc
    }

  def sortDec(): SortDec = {
    expect { case TokParenOpen => () }
    val result = SortDec(
      expect { case TokSymbol(s) => s },
      expect { case TokNumeral(n) => n },
    )
    expect { case TokParenClose => () }
    result
  }

  def maybeSortDec(): Option[SortDec] =
    selectMaybe {
      case TokParenOpen => sortDec()
    }

  @tailrec
  private def starSortDec(acc: Seq[SortDec] = Nil): Seq[SortDec] =
    maybeSortDec() match {
      case Some(value) => starSortDec(acc :+ value)
      case None => acc
    }

  def selector(): (String, Sort) = {
    expect { case TokParenOpen => () }
    val name = expect { case TokSymbol(s) => s }
    val srt = sort()
    expect { case TokParenClose => () }
    (name, srt)
  }

  def maybeSelector(): Option[(String, Sort)] =
    selectMaybe {
      case TokParenOpen => selector()
    }

  def selectors(acc: Seq[(String, Sort)] = Nil): Seq[(String, Sort)] =
    maybeSelector() match {
      case Some(value) => selectors(acc :+ value)
      case None => acc
    }

  def constructor(): Constructor = {
    expect { case TokParenOpen => () }
    val name = expect { case TokSymbol(s) => s }
    val result = Constructor(name, selectors())
    expect { case TokParenClose => () }
    result
  }

  def maybeConstructor(): Option[Constructor] =
    selectMaybe {
      case TokParenOpen => constructor()
    }

  def constructors(acc: Seq[Constructor] = Nil): Seq[Constructor] =
    maybeConstructor() match {
      case Some(value) => constructors(acc :+ value)
      case None => acc
    }

  def datatype(): DataType = {
    expect { case TokParenOpen => () }
    val result = select {
      case TokPar =>
        expect { case TokPar => () }
        expect { case TokParenOpen => () }
        val params = symbols()
        expect { case TokParenClose => () }
        expect { case TokParenOpen => () }
        val cons = constructors()
        expect { case TokParenClose => () }
        DataType(params.map(_.name), cons)
      case TokParenOpen =>
        val cons = constructors()
        expect { case TokParenClose => () }
        DataType(Nil, cons)
    }
    expect { case TokParenClose => () }
    result
  }

  def maybeDatatype(): Option[DataType] =
    selectMaybe {
      case TokParenOpen => datatype()
    }

  @tailrec
  private def starDatatype(acc: Seq[DataType] = Nil): Seq[DataType] =
    maybeDatatype() match {
      case Some(value) => starDatatype(acc :+ value)
      case None => acc
    }

  def z3FlavourDatatype(): Option[(SortDec, DataType)] =
    selectMaybe {
      case TokParenOpen =>
        expect { case TokParenOpen => () }
        val name = expect { case TokSymbol(s) => SortDec(s, 0) }
        val cons = constructors()
        expect { case TokParenClose => () }
        (name, DataType(Nil, cons))
    }

  def z3FlavourDatatypes(acc: Seq[(SortDec, DataType)] = Nil): Seq[(SortDec, DataType)] =
    z3FlavourDatatype() match {
      case Some(value) => z3FlavourDatatypes(acc :+ value)
      case None => acc
    }

  def binding(): Binding = {
    expect { case TokParenOpen => () }
    val name = expect { case TokSymbol(s) => s }
    val t = term()
    expect { case TokParenClose => () }
    Binding(name, t)
  }

  def maybeBinding(): Option[Binding] =
    selectMaybe {
      case TokParenOpen => binding()
    }

  @tailrec
  private def starBinding(acc: Seq[Binding] = Nil): Seq[Binding] =
    maybeBinding() match {
      case Some(value) => starBinding(acc :+ value)
      case None => acc
    }

  def variable(): Var = {
    expect { case TokParenOpen => () }
    val name = expect { case TokSymbol(s) => s }
    val t = sort()
    expect { case TokParenClose => () }
    Var(name, t)
  }

  def maybeVariable(): Option[Var] =
    selectMaybe {
      case TokParenOpen => variable()
    }

  @tailrec
  private def starVariable(acc: Seq[Var] = Nil): Seq[Var] =
    maybeVariable() match {
      case Some(value) => starVariable(acc :+ value)
      case None => acc
    }

  def applyContinuation(id: QualifiedIdentifier): Term =
    Apply(id, term() +: starTerm())

  def parenTerm(): Term =
    select {
      case TokSymbol(s) =>
        consume()
        applyContinuation(UnqualifiedIdentifier(SimpleIdentifier(s)))
      case TokParenOpen =>
        applyContinuation(qualifiedIdentifier())
      case TokAs =>
        consume()
        SortedIdentifier(identifier(), sort())
      case TokUnderscore =>
        consume()
        val name = expect { case TokSymbol(s) => s }
        UnqualifiedIdentifier(IndexedIdentifier(name, index() +: starIndex()))
      case TokLet =>
        consume()
        expect { case TokParenOpen => () }
        val bindings = starBinding()
        expect { case TokParenClose => () }
        Let(bindings, term())
      case TokForall =>
        consume()
        expect { case TokParenOpen => () }
        val vars = variable() +: starVariable()
        expect { case TokParenClose => () }
        Forall(vars, term())
      case TokExists =>
        consume()
        expect { case TokParenOpen => () }
        val vars = variable() +: starVariable()
        expect { case TokParenClose => () }
        Exists(vars, term())
      case TokExclamationPoint =>
        consume()
        val t = term()
        val attrs = starAttribute()
        Attributed(t, attrs)
    }

  def term(): Term =
    select {
      case _: TokNumeral | _: TokDecimal | _: TokHexadecimal | _: TokBinary | _: TokStringLiteral => const()
      case TokSymbol(s) =>
        consume()
        UnqualifiedIdentifier(SimpleIdentifier(s))
      case TokParenOpen =>
        expect { case TokParenOpen => () }
        val result = parenTerm()
        expect { case TokParenClose => () }
        result
    }

  def maybeTerm(): Option[Term] =
    selectMaybe {
      case _: TokNumeral | _: TokDecimal | _: TokHexadecimal | _: TokBinary | _: TokStringLiteral |
           TokParenOpen | _: TokSymbol =>
        term()
    }

  def starTerm(acc: Seq[Term] = Nil): Seq[Term] =
    maybeTerm() match {
      case Some(value) => starTerm(acc :+ value)
      case None => acc
    }

  def info(): GetInfo = {
    expect { case TokSymbol("get-info") => () }
    GetInfo(expect { case TokKeyword(key) => key })
  }

  def setOption(): SetOption = {
    expect { case TokSymbol("set-option") => () }
    SetOption(attribute())
  }

  def declareDataTypes(): DeclareDataTypes = {
    expect { case TokSymbol("declare-datatypes") => () }
    expect { case TokParenOpen => () }
    val sortdecs = starSortDec()
    expect { case TokParenClose => () }
    expect { case TokParenOpen => () }

    if(sortdecs.isEmpty) {
      val result = DeclareDataTypes(z3FlavourDatatypes())
      // expect{ case TokParenClose => () }
      assert(peek.contains(TokParenClose))
      result
    } else {
      val types = starDatatype()
      // expect { case TokParenClose => () }
      // TODO: Shoudl be parse-error?
      assert(peek.contains(TokParenClose))
      assert(sortdecs.length == types.length)
      DeclareDataTypes(sortdecs.zip(types))
    }
  }

  def declareSort(): DeclareSort = {
    expect { case TokSymbol("declare-sort") => () }
    val name = expect { case TokSymbol(s) => s }
    val num = selectMaybe { case TokNumeral(n) => consume(); n }.getOrElse(BigInt(0))
    DeclareSort(SortDec(name, num))
  }

  def declareConst(): DeclareConst = {
    expect { case TokSymbol("declare-const") => () }
    val name = expect { case TokSymbol(s) => s }
    DeclareConst(name, sort())
  }

  def defineSort(): DefineSort = {
    expect { case TokSymbol("define-sort") => () }
    val name = expect { case TokSymbol(s) => s }
    expect { case TokParenOpen => () }
    val args = symbols()
    expect { case TokParenClose => () }
    DefineSort(name, args.map(_.name), sort())
  }

  def defineConst(): DefineConst = {
    expect { case TokSymbol("define-const") => () }
    val name = expect { case TokSymbol(s) => s }
    val t = sort()
    val v = term()
    DefineConst(name, t, v)
  }

  def declareFun(): DeclareFun = {
    expect { case TokSymbol("declare-fun") => () }
    val name = expect { case TokSymbol(s) => s }
    expect { case TokParenOpen => () }
    val args = starSort()
    expect { case TokParenClose => () }
    val res = sort()
    DeclareFun(name, args, res)
  }

  def defineFun(): DefineFun = {
    expect { case TokSymbol("define-fun") => () }
    val name = expect { case TokSymbol(s) => s }
    expect { case TokParenOpen => () }
    val args = starVariable()
    expect { case TokParenClose => () }
    val res = sort()
    val impl = term()
    DefineFun(name, args, res, impl)
  }

  def assrt(): Assert = {
    expect { case TokSymbol("assert") => () }
    Assert(term())
  }

  def push(): Push = {
    expect { case TokSymbol("push") => () }
    Push(selectMaybe { case TokNumeral(n) => n }.getOrElse(BigInt(1)))
  }

  def pop(): Pop = {
    expect { case TokSymbol("pop") => () }
    Pop(selectMaybe { case TokNumeral(n) => n }.getOrElse(BigInt(1)))
  }

  override def next(): SmtCommand = {
    expect { case TokParenOpen => () }
    val result = select {
      case TokSymbol("assert") => assrt()
      case TokSymbol("check-sat") => consume(); CheckSat

      case TokSymbol("push") => push()
      case TokSymbol("pop") => pop()

      case TokSymbol("get-info") => info()
      case TokSymbol("set-option") => setOption()

      case TokSymbol("declare-datatypes") => declareDataTypes()

      case TokSymbol("declare-sort") => declareSort()
      case TokSymbol("define-sort") => defineSort()

      case TokSymbol("declare-const") => declareConst()
      case TokSymbol("define-const") => defineConst()

      case TokSymbol("declare-fun") => declareFun()
      case TokSymbol("define-fun") => defineFun()
    }
    expect { case TokParenClose => () }
    result
  }
}
