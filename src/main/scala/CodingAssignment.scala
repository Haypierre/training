package training

import scala.io.Source
import sys.process._

//Define a case class Transaction which represents a transaction
case class Transaction(
                        transactionId: String,
                        accountId: String,
                        transactionDay: Int,
                        category: String,
                        transactionAmount: Double
                      )

object CodingAssignment {
  //--Helper functions for question 2 & 3
  def sum(transactions: List[Transaction]) : Double = {
    transactions.map(_.transactionAmount).sum
  }

  def average(transactions: List[Transaction]) : Double = {
    sum(transactions) / transactions.size
  }

  def max(transactions: List[Transaction]) : Double = {
    // avoid compilation warning by telling which max we want to use.
    implicit val order = Ordering.Double.IeeeOrdering
    transactions.map(_.transactionAmount).max
  }

  //Answer One
  //Calculate the total transaction value for all transactions for each day.

  def calculateAnswerOne(transactions: List[Transaction]) : List[(Int, Double)]  = {
    transactions
      .groupBy(_.transactionDay) // def groupBy[K](f: (A) => K) : Map[K, List[A]]
      .map(x => (x._1, sum(x._2)))
      .toSeq.sortBy(_._1)
      .toList
  }

  def testAnswerOne(answer1: List[(Int, Double)]): Unit = {
    assert(answer1(0)._2 == 28929.559999999998)
    assert(answer1(16)._2 == 13716.149999999998)
  }

  def formatAnswerOne(answer1: List[(Int, Double)]): Unit = {
    for (element <- answer1) {
      val (day, totalTransactionAmount) = (element._1, element._2)
      if (day < 10)
        //add a space to align the ':'
        println("Day %d  : Total Transaction Amount = %1.2f".format(day, totalTransactionAmount))
      else
        println("Day %d : Total Transaction Amount = %1.2f".format(day, totalTransactionAmount))
    }
  }
  //Answer Two
  //Calculate the average value of transactions per account for each type of transaction (there are seven in
  //total).

  def calculateAnswerTwo(transactions: List[Transaction]) : Map[String, Map[String, Double]] = {
    transactions
      .groupBy(_.accountId)
      .map(element => (element._1, element._2.groupBy(_.category)))
      .map(element => (element._1, element._2.map(element => (element._1, average(element._2)))))
  }

  def testAnswerTwo(answer2: Map[String, Map[String, Double]], path: String): Unit = {
    // Is bash giving the same result than our beloved Scala?
    val t1: String = (s"cat ${path}" #| "grep A11" #| "grep BB" #| "tr ',' ' '" #| "awk '{ total += $5; count++ } END { print total/count }'").!!.trim
    val t2: String = (s"cat ${path}" #| "grep A32" #| "grep DD" #| "tr ',' ' '" #| "awk '{ total += $5; count++ } END { print total/count }'").!!.trim
    assert(answer2("A11")("BB").toString == t1)
    assert(answer2("A32")("DD").toString == t2)
  }

  def formatAnswerTwo(answer2: Map[String, Map[String, Double]]): Unit = {
    val sortableResult = answer2.map(elem => (elem._1.drop(1).toInt, elem._2.toSeq.sortBy(_._1)))
    for (element <- sortableResult.toSeq.sortBy(_._1)) {
      print("Account ID %d -> ".format(element._1))
      for (tuple <- element._2) {
        print("%s=%1.2f ".format(tuple._1, tuple._2))
      }
      println()
    }
  }
  //Answer Three
  //For each day, calculate statistics for each account number for the previous five days of transactions.

  def calculateAnswerThree(transactions: List[Transaction]): List[(Int, String, List[Double])] = {
    //@params: Last Transactions for a given day for a given account
    def calculateStats(transactions: List[Transaction]): Option[List[Double]] = {
      if (transactions.isEmpty)
        None
      else
        Some (
          List(
            max(transactions),
            average(transactions),
            sum(transactions.filter(_.category == "AA")),
            sum(transactions.filter(_.category == "CC")),
            sum(transactions.filter(_.category == "FF"))
          )
        )
    }

    def getLastTransactions(currentDay: Int, daysThreshold: Int, accountId: String) : List[Transaction] = {

      // withFilter: LazyFilter : chaining filter without creating an intermediate list
      transactions
        .withFilter(_.accountId == accountId)
        .withFilter(x => x.transactionDay < currentDay && x.transactionDay >= currentDay - daysThreshold)
        .map(x => x)
    }

    val accountIds : List[String] = transactions.map(_.accountId).distinct
    val days : List[Int] = transactions.map(_.transactionDay).distinct

    for {
      currentDay <- days
      accountId  <- accountIds
    } yield {
        (currentDay, accountId, calculateStats(getLastTransactions(currentDay, 5, accountId)).toList.flatten)
    }
  }

  def testAnswerThree(answer3: List[(Int, String, List[Double])]) : Unit = {
      val t1 = List(537.73, 484.65999999999997, 0, 0, 431.59)
      val r1 = answer3.withFilter(_._1 == 14).withFilter(_._2 == "A1").map(x => x)(0)._3
      val t2 = List()
      val r2 = answer3.withFilter(_._1 == 1).withFilter(_._2 == "A1").map(x=>x)(0)._3

      assert(r1 == t1)
      assert(r2 == t2)
  }

  def formatAnswerThree(answer3: List[(Int, String, List[Double])]) : Unit = {
      //println("No available data for day one!")
      println("Day,Account ID,Maximum,Averge,AA Total Value,CC Total Value,FF Total Value")
      answer3.foreach(day => day match {
        case (day,accountId,Nil) => //if (day > 1) println(s"Day ${day}, No past transactions in the last 5 days for account ${accountId}")
        case (day, accountId, stats) => println(s"%d,%s,%1.2f,%1.2f,%1.2f,%1.2f,%1.2f".format(day, accountId, stats(0), stats(1), stats(2), stats(3), stats(4)))
      })
    }

  //Solution Entry Point
  def solve(path: String): Unit = {
    //The lines of the CSV file (dropping the first to remove the header)
    val transactionslines = Source.fromFile(path).getLines().drop(1)
    //Here we split each line up by commas and construct Transactions
    val transactions: List[Transaction] = transactionslines.map { line =>
      val split = line.split(',')
      Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
    }.toList

    val r1 = calculateAnswerOne(transactions)
    testAnswerOne(r1)
    formatAnswerOne(r1)

    val r2 = calculateAnswerTwo(transactions)
    testAnswerTwo(r2, path)
    formatAnswerTwo(r2)

    val r3 = calculateAnswerThree(transactions)
    testAnswerThree(r3)
    formatAnswerThree(r3)
  }
}