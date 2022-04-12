package smtparse.parse

sealed trait SmtToken
case object TokParenOpen extends SmtToken
case object TokParenClose extends SmtToken

case class TokNumeral(n: BigInt) extends SmtToken
case class TokDecimal(n: BigDecimal) extends SmtToken
case class TokHexadecimal(data: String) extends SmtToken
case class TokBinary(data: String) extends SmtToken
case class TokStringLiteral(data: String) extends SmtToken

case class TokSymbol(name: String) extends SmtToken
case class TokKeyword(key: String) extends SmtToken

sealed trait SmtReservedSymbol extends SmtToken
case object TokExclamationPoint extends SmtReservedSymbol
case object TokUnderscore extends SmtReservedSymbol
case object TokAs extends SmtReservedSymbol
case object TokKwdBinary extends SmtReservedSymbol
case object TokKwdDecimal extends SmtReservedSymbol
case object TokExists extends SmtReservedSymbol
case object TokKwdHexadecimal extends SmtReservedSymbol
case object TokForall extends SmtReservedSymbol
case object TokLet extends SmtReservedSymbol
case object TokMatch extends SmtReservedSymbol
case object TokKwdNumeral extends SmtReservedSymbol
case object TokPar extends SmtReservedSymbol
case object TokKwdString extends SmtReservedSymbol