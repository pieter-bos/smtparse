package smtparse.output

import smtparse.parse.{DeclareConst, DeclareFun, DefineConst, DefineFun, SmtCommand}

sealed trait Dialect {
  def symbol(f: String): String

  def declareConst(d: DeclareConst): SmtCommand = d
  def defineConst(d: DefineConst): SmtCommand = d
}

case object Dialect {
  case object Z3 extends Dialect {
    override def symbol(f: String): String = f
  }

  case object CVC5 extends Dialect {
    override def symbol(f: String): String = f match {
      case "implies" => "=>"
      case other => other
    }
  }

  case object Vampire extends Dialect {
    override def symbol(f: String): String = f

    override def declareConst(d: DeclareConst): SmtCommand =
      DeclareFun(d.name, Nil, d.sort)
    override def defineConst(d: DefineConst): SmtCommand =
      DefineFun(d.name, Nil, d.sort, d.value)
  }
}