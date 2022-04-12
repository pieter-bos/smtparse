package smtparse.parse

sealed trait Term
sealed trait SExpr

sealed trait Const extends Term with SExpr
sealed trait Index
case class Numeral(n: BigInt) extends Const with Index
case class SmtDecimal(n: BigDecimal) extends Const
case class Hex(data: String) extends Const
case class Bin(data: String) extends Const
case class SmtString(data: String) extends Const

case class Symbol(name: String) extends Term with SExpr with Index
case class Keyword(name: String) extends Term with SExpr
case class Reserved(symbol: SmtReservedSymbol) extends SExpr

case class Apply(f: QualifiedIdentifier, args: Seq[Term]) extends Term
case class SExprApply(args: Seq[SExpr]) extends SExpr

sealed trait Identifier
case class SimpleIdentifier(name: String) extends Identifier
case class IndexedIdentifier(name: String, indices: Seq[Index]) extends Identifier

sealed trait QualifiedIdentifier extends Term
case class UnqualifiedIdentifier(id: Identifier) extends QualifiedIdentifier
case class SortedIdentifier(id: Identifier, sort: Sort) extends QualifiedIdentifier

case class Binding(name: String, value: Term)
case class Var(name: String, t: Sort)
//case class Pattern(names: Seq[String])

case class Let(bindings: Seq[Binding], body: Term) extends Term
case class Forall(vars: Seq[Var], body: Term) extends Term
case class Exists(vars: Seq[Var], body: Term) extends Term
//case class Match(e: Term, cases: Seq[(Pattern, Term)]) extends Term
case class Attributed(e: Term, attrs: Seq[Attribute]) extends Term

sealed trait Sort
case class SimpleSort(name: Identifier) extends Sort
case class SortApply(name: Identifier, args: Seq[Sort]) extends Sort

case class Attribute(key: String, value: Option[SExpr])

case class SortDec(name: String, n: BigInt)

case class Constructor(name: String, selectors: Seq[(String, Sort)])
case class DataType(parameters: Seq[String], constructors: Seq[Constructor])

sealed trait SmtCommand
case class GetInfo(key: String) extends SmtCommand
case class SetOption(attr: Attribute) extends SmtCommand
case class DeclareDataTypes(decls: Seq[(SortDec, DataType)]) extends SmtCommand
case class DeclareSort(decl: SortDec) extends SmtCommand
case class DefineSort(name: String, args: Seq[String], defn: Sort) extends SmtCommand
case class DeclareConst(name: String, sort: Sort) extends SmtCommand
case class DefineConst(name: String, sort: Sort, value: Term) extends SmtCommand
case class DeclareFun(name: String, args: Seq[Sort], res: Sort) extends SmtCommand
case class DefineFun(name: String, args: Seq[Var], res: Sort, impl: Term) extends SmtCommand
case class Assert(t: Term) extends SmtCommand
case object CheckSat extends SmtCommand
case class Push(n: BigInt) extends SmtCommand
case class Pop(n: BigInt) extends SmtCommand