package training
import scala.annotation.tailrec

object Sorted {
  def f(a: Int, b: Int): Boolean = a <= b

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val maxIndexSize = as.size - 1

    @tailrec def loop(index: Int): Boolean = index match {
      case `maxIndexSize` => ordered(as(index - 1), as(index))
      case _ => if (!ordered(as(index - 1), as(index))) false else loop(index + 1)
    }

    loop(1)
  }
}
