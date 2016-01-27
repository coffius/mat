package io.koff.risk_ident

/**
 * Information about customer orders and their time
 * @param time time when orders were created
 * @param customerOrders list of orders that were created at this time
 */
case class TimeAndOrders(time: Int, customerOrders: Seq[Int])
