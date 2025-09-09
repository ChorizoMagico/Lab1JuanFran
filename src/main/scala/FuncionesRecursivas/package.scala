package object FuncionesRecursivas {

  def maxLin(l: List[Int ]):Int = {

    def maxLinFunction(list:List[Int]):Int = {

      def nextList = list.tail
      def actualNumber = list.head

      if (nextList.isEmpty) actualNumber else {
        mayorOIgualQue(actualNumber, maxLinFunction(nextList))
      }
    }

    def mayorOIgualQue(first: Int, second: Int): Int = {
      if(first >= second) first else second
    }

    maxLinFunction(l)
  }

  def maxIt(l: List[Int]): Int = {

    def maxItFunction(actualList: List[Int], maxNumber: Int): Int = {

      def nextList = actualList.tail

      def actualNumber = actualList.head

      if (nextList.isEmpty) mayorOIgualQue(maxNumber, actualNumber) else {
        maxItFunction(nextList, mayorOIgualQue(maxNumber, actualNumber))
      }
    }

    def mayorOIgualQue(first: Int, second: Int): Int = {
      if (first >= second) first else second
    }

    maxItFunction(l, l.head)
  }


}