package smtsolve

import smtsolve.smt.{Lex, Parse}

import java.io.{File, FileInputStream, InputStreamReader}
import java.nio.charset.Charset

case object Main {
  def main(args: Array[String]): Unit = {
    val in = new InputStreamReader(new FileInputStream(new File("/home/pieter/tmp/logfile-00.smt2")), "UTF-8")

    for(command <- Parse(in)) {
      println(command)
    }

    in.close()
  }
}
