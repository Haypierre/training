package training

import scala.collection.mutable.Map

object TwoSum {
    def compute(numbers: Array[Int], target: Int) : Option[(Int, Int)] = {

        val cache = Map[Int, Int]()

        for (i <- 0 until numbers.length) {

            val current = numbers(i)
            if (cache.contains(current))
                return Some(cache(current), i) 
            cache.addOne((target - current), i)
        }

        return None
    }
}
