package object FuncionesRecursivas {

  def maxLin(l: List[Int ]):Int = {

    def maxLinFunction(newList:List[Int]):Int = {

      def nextList = newList.tail
      def actualNumber = newList.head

      if (nextList.isEmpty) actualNumber else {

        def nextNumber = nextList.head
        val linList = mayorOIgualQue(actualNumber, nextNumber)::nextList.tail
        maxLinFunction(linList)
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

      if (nextList.isEmpty) maxNumber else {
        def nextNumber = nextList.head

        maxItFunction(nextList, mayorOIgualQue(maxNumber, nextNumber))
      }
    }

    def mayorOIgualQue(first: Int, second: Int): Int = {
      if (first >= second) first else second
    }

    maxItFunction(l, l.head)
  }


}