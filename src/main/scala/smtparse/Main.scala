package smtparse

import smtparse.output.Dialect
import smtparse.parse.{Lex, Numeral, Parse, SExpr, SmtString, Symbol}

import java.io.{File, FileInputStream, InputStreamReader}
import java.nio.charset.{Charset, StandardCharsets}
import scala.concurrent.ExecutionContext.Implicits.global

case object Main {
  def main(args: Array[String]): Unit = {
    val tt = Symbol("true")
    val ff = Symbol("false")
    val zero = Numeral(0)

    val z3SiliconDefault = PinnedOptionsVerifier(Seq("z3", "-smt2", "-in"), Dialect.Z3, Map[String, SExpr](
      "global-decls" -> tt,
      "auto_config" -> ff,
      "smt.restart_strategy" -> zero,
      "smt.restart_factor" -> Symbol("1.5"),
      "smt.case_split" -> Numeral(3),
      "smt.delay_units" -> tt,
      "smt.delay_units_threshold" -> Numeral(16),
      "nnf.sk_hack" -> tt,
      "type_check" -> tt,
      "smt.bv.reflect" -> tt,
      "smt.mbqi" -> ff,
      "smt.qi.eager_threshold" -> Numeral(100),
      "smt.qi.cost" -> SmtString("(+ weight generation)"),
      "smt.qi.max_multi_patterns" -> Numeral(1000),
      "smt.phase_selection" -> zero,
      "sat.phase" -> Symbol("caching"),
      "sat.random_seed" -> zero,
      "nlsat.randomize" -> tt,
      "nlsat.seed" -> zero,
      "nlsat.shuffle_vars" -> ff,
      "fp.spacer.order_children" -> zero,
      "fp.spacer.random_seed" -> zero,
      "smt.arith.random_initial_value" -> tt,
      "smt.random_seed" -> zero,
      "sls.random_offset" -> tt,
      "sls.random_seed" -> zero,
      "sls.restart_init" -> ff,
      "sls.walksat_ucb" -> tt,
      "model.v2" -> tt,
    ).map { case (k, v) => k -> Some(v) })

    val cvc5 = PinnedOptionsVerifier(Seq("cvc5", "-"), Dialect.CVC5, Map.empty)

    val vampire = PinnedOptionsVerifier(Seq("vampire",
      "--input_syntax", "smtlib2",
      "--output_mode", "smtcomp",
      // "-t", "3",
    ), Dialect.Vampire, Map.empty)

    val z3cvc5M = new Multiplexer(Seq(z3SiliconDefault, cvc5))
    val z3M = new Multiplexer(Seq(z3SiliconDefault))
    val vampireM = new Multiplexer(Seq(vampire))
    val multiplexer = vampireM
    multiplexer.options.put("print-success", Some(ff))

    for(command <- Parse(new InputStreamReader(System.in, StandardCharsets.UTF_8))) {
      System.out.println(s"; " + command)
      val res = multiplexer.addCommand(command)
      System.out.print(res)
    }
  }
}
