package io.koff.risk_ident

import java.io.File

import scala.annotation.tailrec
import scala.io.Source

/**
 * Schedule orders to minimize average waiting time
 */
object OrderScheduler {

  /**
   * Helper for work with either seq
   */
  object EitherHelper {
    /**
     * Converts Seq[Either[A, B]] to Either[A, Seq[B]] with a fail-fast strategy
     */
    def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] = {
      s.foldLeft(Right(Nil): Either[A, List[B]]) {
        (accum, either) => for {
          accValue <- accum.right
          value <- either.right
        } yield accValue :+ value
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

  /**
   * Returns iterator for file's lines if file exists
   * @param file a file
   * @return iterator with lines or error string
   */
  private def readFile(file: File): Either[String, Iterator[String]] = {
    if(file.isFile && file.canRead) {
      Right(Source.fromFile(file, "utf-8").getLines())
    } else {
      Left(s"can't read file[${file.getAbsolutePath}]")
    }
  }

  /**
   * Reads lines from `lineIter` and check their number
   * @param lineIter iterator with lines
   * @param number needed number of lines
   * @return sequence of readed lines or error string if the number of lines in iterator is less then `number`
   */
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

  /**
   * Calculate minimum total time of waiting
   * @param currentCookTime current time of a cook
   * @param currentOrders available orders for cooking
   * @param futureOrders orders that will be available
   * @param totalTime current value of total waiting time
   * @return minimum total time of waiting
   */
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

  /**
   * Finds the index of the order with minimal cookDuration
   * @param currentOrders list of orders
   * @return index of the order with minimal cookDuration
   */
  def findMinDurationIndex(currentOrders: List[PizzaOrder]): Int = findMinIndex(Int.MaxValue, 0, 0, currentOrders.iterator)

  /**
   * Custom search of a min elem.
   * @param minVal current min value
   * @param minIndex the index of current min value
   * @param currIndex current index in the seq
   * @param iter iterator with data
   * @return the index of current min value
   */
  @tailrec
  def findMinIndex(minVal: Int, minIndex: Int, currIndex: Int, iter: Iterator[PizzaOrder]): Int = {
    if(iter.hasNext){
      val next = iter.next()
      if(next.cookDuration < minVal){
        findMinIndex(next.cookDuration, currIndex, currIndex + 1, iter)
      } else {
        findMinIndex(minVal, minIndex, currIndex + 1, iter)
      }
    } else {
      minIndex
    }
  }

  /**
   * Removes an elem from the list by index and returns a new list without the removed element
   * @param list list
   * @param index index to remove
   * @return a new list without the removed element
   */
  def deleteElemByIndex[T](list: List[T], index: Int): List[T] = {
    list.take(index) ++ list.drop(index + 1)
  }
}
