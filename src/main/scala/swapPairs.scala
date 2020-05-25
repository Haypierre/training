package training
import scala.reflect.ClassTag

object Swap {

    def swapPairs[A:ClassTag](arr: Array[A]) : Array[A] = {

        arr.grouped(2).flatMap {
            case Array(x,y) => Array(y,x)
            case Array(x) => Array(x)
            }.toArray
       
    }
}