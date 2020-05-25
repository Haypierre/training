package training
import scala.annotation.tailrec

//Translate Roman numbers to Integers

object RomanNumbers {

    def romanToInt(s: String) : Int = {

        val binary : Map[String, Int] = Map(
                "IV" -> 4,
                "IX" -> 9,
                "XL" -> 40,
                "XC" -> 90,
                "CD" -> 400,
                "CM" -> 900
            )

        val unary : Map[Char, Int] = Map(
                'I' -> 1,
                'V' -> 5,
                'X' -> 10,
                'L' -> 50,
                'C' -> 100,
                'D' -> 500,
                'M' -> 1000
        )

        @tailrec
        def helper(s: String, acc: Int) : Int = {

            if (s.isEmpty)
                return acc

            val k = s.slice(0,2)
            if (binary.contains(k))
                helper(s.slice(2, s.size), acc + binary(k))
            else
                helper(s.tail, acc + unary(k(0)))
        }

        helper(s,0)
    }
}
