package io.koff.risk_ident

import org.scalatest.{Matchers, BeforeAndAfterAll, FreeSpec}

/**
 * Spec for OrderScheduler
 */
class OrderSchedulerSpec 
  extends FreeSpec
  with Matchers
  with BeforeAndAfterAll
{
  "should delete elem by Id" in {
    val initList = List(1,2,3,4,5,6)
    val index = 3
    val resultList = OrderScheduler.deleteElemByIndex(initList, index)
    resultList shouldBe List(1,2,3,5,6)
  }

  "should find an order with minimal duration" in {
    val list = List(
      PizzaOrder(1, 10),
      PizzaOrder(1, 3),
      PizzaOrder(1, 5),
      PizzaOrder(1, 1)
    )

    val index = OrderScheduler.findMinDurationIndex(list)
    index shouldBe 3
  }

  "should calc total and average time" - {
    "test#1" in {
      val orders = List(
        PizzaOrder(0, 9),
        PizzaOrder(0, 1),
        PizzaOrder(20, 5)
      )

      val totalTime = OrderScheduler.calcTotalWaitTime(orders)
      totalTime shouldBe 16

      val averageTime = OrderScheduler.calcMinimumAverageTime(orders)
      averageTime shouldBe 5
    }

    "test#2" in {
      val orders = List(
        PizzaOrder(1, 3),
        PizzaOrder(3, 6),
        PizzaOrder(2, 9)
      )

      val totalTime = OrderScheduler.calcTotalWaitTime(orders)
      totalTime shouldBe 27

      val averageTime = OrderScheduler.calcMinimumAverageTime(orders)
      averageTime shouldBe 9
    }
  }

  "should parse an order line correctly" in {
    val pizzaOrderResult = OrderScheduler.parseOrderLine("0  1")
    pizzaOrderResult shouldBe Right(PizzaOrder(0, 1))
  }

  "should return error" -{
    "test #1" in {
      val wrongLine = "0 1"
      val pizzaOrderResult = OrderScheduler.parseOrderLine(wrongLine)
      pizzaOrderResult shouldBe a[Left[_, _]]
    }

    "test #2" in {
      val wrongLine = "0 wrongNumber"
      val pizzaOrderResult = OrderScheduler.parseOrderLine(wrongLine)
      pizzaOrderResult shouldBe a[Left[_, _]]
    }
  }
}
