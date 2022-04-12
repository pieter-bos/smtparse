package smtparse.output

sealed trait Dialect {
  def symbol(f: String): String
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
}