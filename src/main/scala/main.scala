

def maxIter(l: List[Int ]):Int = {

  def maxIterFunction(actualList:List[Int], maxNumber:Int):Int = {

    def nextList = actualList.tail
    def actualNumber = actualList.head

    if (nextList.isEmpty) maxNumber else {
      def nextNumber = nextList.head

      maxIterFunction(nextList, mayorOIgualQue(maxNumber, nextNumber))
    }
  }

  def mayorOIgualQue(first: Int, second: Int): Int = {
    if(first >= second) first else second
  }

  maxIterFunction(l, l.head)
}