package training
import scala.collection.mutable.Stack

object BracketChecker {
    
    val openings = "({["

    def complementary(c1: Char, c2: Char) : Boolean = {
        (c1, c2) match {
            case ('(', ')') => true
            case ('{', '}') => true
            case ('[', ']') => true
            case _ => false
        }
    }

    def isValid(s: String): Boolean = {
        
        val stack = Stack[Char]()
        
        for (c <- s) {
            if(openings.contains(c)) {
                stack.push(c)
            }
            else {
                if (stack.isEmpty) return false
                val lastOpen = stack.pop
                if (!complementary(lastOpen, c))
                    return false
            }
        }    
        stack.isEmpty
    }
}