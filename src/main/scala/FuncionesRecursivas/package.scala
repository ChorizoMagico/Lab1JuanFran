package object FuncionesRecursivas {

  def maxLin(l: List[Int ]):Int = {

    def maxLinFunction(list:List[Int]):Int = {

      if (list.tail.isEmpty) list.head else {
        mayorOIgualQue(list.head, maxLinFunction(list.tail))
      }
    }

    def mayorOIgualQue(first: Int, second: Int): Int = {
      if(first >= second) first else second
    }

    maxLinFunction(l)
  }

  def maxIt(l: List[Int]): Int = {

    def maxItFunction(actualList: List[Int], maxNumber: Int): Int = {

      if (actualList.tail.isEmpty) mayorOIgualQue(maxNumber, actualList.head) else {
        maxItFunction(actualList.tail, mayorOIgualQue(maxNumber, actualList.head))
      }
    }

    def mayorOIgualQue(first: Int, second: Int): Int = {
      if (first >= second) first else second
    }

    maxItFunction(l, l.head)
  }

  def movsTorresHanoi(n: Int): BigInt = {
    if (n == 1) 1
    else 2 * movsTorresHanoi(n - 1) + 1
  }

  def torresHanoi(n: Int, t1: Int, t2: Int, t3: Int): List[(Int, Int)] = {
    if (n == 1) {
      List((t1, t3))
    } else {

      val paso1 = torresHanoi(n - 1, t1, t3, t2)

      val paso2 = List((t1, t3))

      val paso3 = torresHanoi(n - 1, t2, t1, t3)

      paso1 ++ paso2 ++ paso3
    }
  }

}