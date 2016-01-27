package io.koff.risk_ident

import java.io.File

object Main {
  def main(args: Array[String]) {
    val result = OrderScheduler.readFileAndCalcMAT(new File("sample_data.txt"))
    result match {
      case Left(err) => println(s"error: [$err]")
      case Right(mat) => println(s"result: $mat")
    }
  }
}
