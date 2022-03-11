package smtsolve.smt

sealed trait Term

sealed trait SmtConst extends Term
case class SmtNumeral(n: BigInt) extends SmtConst
case class SmtDecimal(n: BigDecimal) extends SmtConst
case class SmtHex(data: String) extends SmtConst
case class SmtBin(data: String) extends SmtConst
case class SmtString(data: String) extends SmtConst

case class Symbol(name: String) extends Term
case class Keyword(name: String) extends Term

case class Attribute(key: String, value: Option[Term])

sealed trait SmtCommand
case class GetInfo(key: String) extends SmtCommand
case class SetOption(key: String, value: Attribute) extends SmtCommand