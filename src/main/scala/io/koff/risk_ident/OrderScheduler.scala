package io.koff.risk_ident

import java.io.File

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.io.Source

/**
 * Schedule orders to minimize average waiting time
 */
object OrderScheduler {

  object EitherHelper {
    def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] = {
      s.foldRight(Right(Nil): Either[A, List[B]]) {
        (either, accum) => for {
          accValue <- accum.right
          value <- either.right
        } yield value :: accValue
      }
    }
  }

  /**
   * Reads file and calculate minimum average time
   * @param file file with order data
   */
  def readFileAndCalcMAT(file: File): Either[String, BigInt] = {
    for{
      lineIter <- readFile(file).right
      number <- parseValue(lineIter.next()).right
      lines <- readLines(lineIter, number).right
      orders <- EitherHelper.sequence(lines.map(parseOrderLine)).right
    } yield {
      calcMinimumAverageTime(orders)
    }
  }

  private def readFile(file: File): Either[String, Iterator[String]] = {
    if(file.isFile && file.canRead) {
      Right(Source.fromFile(file, "utf-8").getLines())
    } else {
      Left(s"can't read file[${file.getAbsolutePath}]")
    }
  }

  private def readLines(lineIter: Iterator[String], number: Int): Either[String, Seq[String]] = {
    val seq = lineIter.take(number).toSeq
    if(seq.size < number){
      Left(s"wrong number of orders: ${seq.size} instead of $number")
    } else {
      Right(seq)
    }
  }

  /**
   * Parse a string as a PizzaOrder<br/>
   * Format:"```time_int[space][space]duration_int```"
   * @param line a string to parse
   * @return a pizza order or string with error
   */
  def parseOrderLine(line: String): Either[String, PizzaOrder]= {
    val parts = line.split("  ")
    if(parts.size != 2){
      Left(s"wrong line format: line should contain 2 numbers: [$line]")
    } else {
      for{
        time <- parseValue(parts(0)).right
        duration <- parseValue(parts(1)).right
      } yield {
        PizzaOrder(time, duration)
      }
    }
  }

  /**
   * Parse a string as Int value and catch possible exceptions
   * @param string a string to parse
   * @return int or error string
   */
  private def parseValue(string: String): Either[String, Int] = try {
    Right(string.toInt)
  } catch {
    case err: NumberFormatException => Left(s"wrong number format: [$string]")
  }

  /**
   * Calc minimum average time for all orders
   * @param orders orders for calculation
   * @return minimum average time for orders or error string
   */
  def calcMinimumAverageTime(orders: Seq[PizzaOrder]): BigInt = {
    val totalTime = calcTotalWaitTime(orders)
    totalTime / orders.size
  }

  /**
   * Calc total wait time for all orders
   * @param orders orders for calculation
   * @return minimum average time for orders or error string
   */
  def calcTotalWaitTime(orders: Seq[PizzaOrder]): BigInt = {
    val groupedByTime = orders.groupBy(_.time).toList
    val sortedByTime = groupedByTime.sortBy{ case (time, _) => time }
    val sortedByOrderDuration = sortedByTime.flatMap {
      case (_, groupedOrders) => groupedOrders.sortBy(_.cookDuration)
    }
    val firstOrder :: tail = sortedByOrderDuration
    cookNext(firstOrder.time, List(firstOrder), tail, 0)
  }

  @tailrec
  private def cookNext(currentCookTime: Int, currentOrders: List[PizzaOrder], futureOrders: List[PizzaOrder], totalTime: BigInt): BigInt = {
    if(currentOrders.isEmpty && futureOrders.isEmpty) {
      //all pizzas are cooked
      totalTime
    } else if(currentOrders.isEmpty && futureOrders.nonEmpty) {
      //don't have any orders right now. So wait the next order
      val nextOrder :: tail = futureOrders
      cookNext(currentCookTime, List(nextOrder), tail, totalTime)
    } else {
      //cooking and calculating
      val minIndex: Int = findMinDurationIndex(currentOrders)
      val orderToCook = currentOrders(minIndex)
      
      val inQueueTime = if(currentCookTime >= orderToCook.time) { 
        currentCookTime - orderToCook.time
      } else {
        0
      }
      
      val waitTime = inQueueTime + orderToCook.cookDuration
      val remainOrders = deleteElemByIndex(currentOrders, minIndex)
      val nextCookTime = currentCookTime + orderToCook.cookDuration
      val (nextOrders, inFuture) = futureOrders.span(_.time <= nextCookTime)

      //cook the next pizza
      cookNext(nextCookTime, remainOrders ++ nextOrders, inFuture, totalTime + waitTime)
    }
  }

  def findMinDurationIndex(currentOrders: List[PizzaOrder]): Int = {
    currentOrders.zipWithIndex.minBy(_._1.cookDuration)._2
  }

  def deleteElemByIndex[T](list: List[T], index: Int): List[T] = {
    list.take(index) ++ list.drop(index + 1)
  }
}
