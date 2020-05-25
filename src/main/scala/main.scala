import training._

//Training Entry Point.
//Problems ranged from easy to hard.
//Some come from LeetCode, others from Coding Interviews / Assignement.


object main extends App {

    //swap pairs in an array
    println(Swap.swapPairs((1 to 100).toArray).mkString(" "))
    //Check if an array of generic type is sorted.
    println(Sorted.isSorted(Array(9,1,34), Sorted.f))
    //fibonacci function - tailrec flavor
    println(Fibonacci.fib(15))
    //stock price
    println(Minmax.maxProfit(Array(1,5,145,34,56,24,567)))
    //bracket validator
    println(BracketChecker.isValid("()"))
    println(BracketChecker.isValid("({)"))
    println(BracketChecker.isValid("({[]})")) 
    //roman to integer
    println(RomanNumbers.romanToInt("MMMXLV"))
    //Google meetings
    val freeSlots = Meetings.find(List(("9:00", "10:00"), ("11:00", "12:30"), ("14:30", "17:00")),
    List( ("8:10","10:30"),("10:30","12:00"),("16:30","18:00") ))
    println(freeSlots)
    //computation of Transactions data to extract useful information
    CodingAssignment.solve("src/main/resources/transactions.txt") 

}
