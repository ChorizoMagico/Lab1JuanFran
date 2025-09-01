package object FuncionesRecursivas {
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