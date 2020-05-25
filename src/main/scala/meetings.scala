package training

//Google interview question
//Given a list of scheduled meetings of two employees, find all possible free time slots for them to have a meeting.
//l1 = [[9:00,10:00], [11:00,12:30], [14:30,17:00]]
//l2 = [[8:10,10:30], [10:30,12:00], [16:30,18:00]]
//output = [12:30, 14:30]

object Meetings {

    val meetingDuration = 30
    
    def find(l1: List[(String, String)], l2: List[(String, String)]) : List[(String, String)] = {
        
        val merged = l1 zip l2 map {
            case ((a:String, b:String), (c:String, d:String))
                    => (compareTimes(a,c,min), compareTimes(b,d,max)) 
        }

        merged.sliding(2,1).collect {
            case List(x,y) if (timeToInt(y._1) - timeToInt(x._2) >= meetingDuration) => (x._2, y._1)    
        }.toList
    }

    def min(a: Int, b: Int) = a < b

    def max(a: Int, b: Int) = a > b

    def timeToInt(t: String) : Int = {
        t.split(':') match {
            case Array(x,y) => x.toInt * 60 + y.toInt
            case _ => throw(new Exception("Invalid meeting format"))
        }
    }

    def compareTimes(t1: String, t2: String, ordered: (Int, Int) => Boolean) : String = {
        val _t1 = timeToInt(t1)
        val _t2 = timeToInt(t2)

        if (ordered(_t1, _t2)) t1 else t2
    }

}
