package training

object Minmax {
    def maxProfit(prices: Array[Int]): Int = {
        
        var min = Integer.MAX_VALUE
        var max = 0
        
        for (price <- prices) {
            if (price < min)
                min = price
            else
                max = Math.max(max, price - min)
        }
        
        max
    }

    def main(args: Array[String]) {
        println(maxProfit(Array(1,5,145,34,56,24,567)))
    }
}