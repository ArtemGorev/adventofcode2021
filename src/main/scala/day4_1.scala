//import scala.annotation.tailrec
//import scala.io.Source
//
//object day4_1 extends App {
//
//  type Ticket = List[List[Int]]
//
//  val filename = "inputs/day4_1.txt"
//  val source   = Source.fromFile(filename)
//  val lines    = source.getLines().toList
//
//  val orderToDrawNumbers  = lines.head.split(",").map(n => n.trim.toInt).toList
//  val dimensionSize       = lines(3).split(" ").count(x => !x.isBlank)
//  val emptyStringFilterFn = (x: String) => !x.isBlank
//
//  val tickets: List[List[List[Int]]] = lines
//    .slice(2, lines.length)
//    .filter(emptyStringFilterFn)
//    .grouped(dimensionSize)
//    .map(a => a.map(b => b.split(" ").filter(emptyStringFilterFn).map(x => x.toInt).toList))
//    .toList
//
//  def calculateTicketScore(input: (Ticket, List[Int])) = {
//    val ticket                     = input._1
//    val drawnNumbers               = input._2
//    val numbersOfFirstWinnerTicket = ticket.reduce((a, b) => a.concat(b))
//    val uncheckedNumbers           = numbersOfFirstWinnerTicket.filterNot(drawnNumbers.contains)
//    val sum                        = uncheckedNumbers.sum
//    val last                       = drawnNumbers.last
//    println(s"sum => $sum, last => $last")
//    sum * last
//  }
//
//  def isWinner(ticket: Ticket, drawnNumber: List[Int]): Boolean =
//    checkRows(ticket, drawnNumber) || checkColumns(ticket, drawnNumber)
//
//  def checkColumns(ticket: Ticket, drawnNumber: List[Int]): Boolean =
//    checkRows(ticket.transpose, drawnNumber)
//
//  def checkRows(ticket: Ticket, drawnNumber: List[Int]): Boolean =
//    ticket
//      .map(row => row.map(drawnNumber.contains).forall(b => b))
//      .reduce((a, b) => a || b)
//
//  @tailrec
//  def _findTicket(tickets: List[Ticket], nextNumbers: List[Int], drawnNumbers: List[Int]): (Ticket, List[Int]) = {
//    tickets.find(ticket => isWinner(ticket, drawnNumbers)) match {
//      case Some(ticket) => (ticket, drawnNumbers)
//      case None         => _findTicket(tickets, nextNumbers.tail, drawnNumbers :+ nextNumbers.head)
//    }
//
//  }
//
//  def findFirstWinningTicket(tickets: List[Ticket], order: List[Int]): (Ticket, List[Int]) =
//    _findTicket(tickets, order.tail, List(order.head))
//
//  @tailrec
//  def _findLastTicket(tickets: List[Ticket], drawnNumbers: List[Int], prevNumber: Int): (Ticket, List[Int]) = {
//    tickets.find(ticket => !isWinner(ticket, drawnNumbers)) match {
//      case Some(ticket) if isWinner(ticket, drawnNumbers :+ prevNumber) =>
//        (ticket, drawnNumbers :+ prevNumber)
//      case _ =>
//        _findLastTicket(tickets, drawnNumbers.tail, drawnNumbers.head)
//    }
//  }
//
//  def findLastWinningTicket(tickets: List[Ticket], orderToDrawNumbers: List[Int]) =
//    _findLastTicket(tickets, orderToDrawNumbers.reverse, -1)
//
//  println(s"firstPartResult => ${calculateTicketScore(findFirstWinningTicket(tickets, orderToDrawNumbers))}")
//  println(s"secondPartResult => ${calculateTicketScore(findLastWinningTicket(tickets, orderToDrawNumbers))}")
//
//}
