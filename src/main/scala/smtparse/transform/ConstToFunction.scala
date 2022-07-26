package smtparse.transform

import smtparse.parse.{DefineConst, DefineFun}

case object ConstToFunction {
  def transform(c: DefineConst): DefineFun = DefineFun(c.name, Nil, c.sort, c.value)
}
