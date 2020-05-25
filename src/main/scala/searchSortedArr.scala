package training
object BinarySearch {
    
    
    def binarySearchIterative(list: Array[Int], target: Int): Int = {
        var left = 0
        var right = list.length-1
        while (left<=right) {
         val mid = left + (right-left)/2
        if (list(mid)==target)
         return mid
        else if (list(mid)>target)
          right = mid-1
        else
          left = mid+1
        }
        -1
    }
    
    def search(nums: Array[Int], target: Int): Int = {
       
        
        var pivot = 0
        for (i <- 0 until nums.size -1) if (nums(i + 1) < nums(i)) pivot = i
        
        val arr = if (target >= pivot) nums.slice(0,pivot+1) else nums.slice(pivot,nums.size)

        println(arr.mkString(" "))
        
        
        binarySearchIterative(arr, target)        
    }
}