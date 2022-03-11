package smtsolve.smt

sealed trait SmtToken
case object ParenOpen extends SmtToken
case object ParenClose extends SmtToken
case class Numeral(n: BigInt) extends SmtToken
case class Decimal(n: BigDecimal) extends SmtToken
case class Hexadecimal(data: String) extends SmtToken
case class Binary(data: String) extends SmtToken
case class StringLiteral(data: String) extends SmtToken
case class Symbol(name: String) extends SmtToken
case class Keyword(key: String) extends SmtToken