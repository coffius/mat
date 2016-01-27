package io.koff.risk_ident

import java.io.File

object Main {
  def main(args: Array[String]) {
    val startTime = System.currentTimeMillis()
    val result = OrderScheduler.readFileAndCalcMAT(new File("big_data.txt"))
    val diff = System.currentTimeMillis() - startTime
    result match {
      case Left(err) => println(s"error: [$err]")
      case Right(mat) => println(s"result: $mat. time: $diff msec")
    }
  }
}
