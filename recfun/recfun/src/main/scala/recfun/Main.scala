package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || r == 1 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val OPEN_BRACKET = '('
    val CLOSED_BRACKET = ')'

    val charsString = chars.mkString

    def getExpr(): Option[(String, Int)] = {
      val openAt = charsString.lastIndexOf(OPEN_BRACKET)
      val closedAt = charsString.indexOf(CLOSED_BRACKET, openAt)

      val noBrackets = openAt == -1 && closedAt == -1;
      val notBalanced = (openAt == -1 && closedAt != -1) || (openAt != -1 && closedAt == -1)
      val wrongOrder = openAt > closedAt
      if (noBrackets) {
        Some(("", -1))
      } else if (notBalanced || wrongOrder) {
        None
      } else Some((charsString.slice(openAt, closedAt + 1), openAt))
    }


    val exprTuple = getExpr()

    exprTuple match {
      case Some((expr: String, index: Int)) => {
        if (expr.isEmpty) true
        else {
          val restExpr = charsString.replace(expr, "")
          balance(restExpr.toList)
        }
      }
      case _ => false
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.isEmpty) 0
    else {
      val descCoins = coins.sorted.reverse
      val biggestCoin = descCoins.head
      lazy val restChange = countChange(money, descCoins.tail)
      biggestCoin match {
        case c if c == 0 || c > money => restChange
        case c if c == money => 1 + restChange
        case _ =>
          val maxBiggestCoinsInMoney = (money - (money % biggestCoin)) / biggestCoin
          if (coins.size == 1 && maxBiggestCoinsInMoney * biggestCoin == money) 1
          else {
            val startingSums = (1 to maxBiggestCoinsInMoney).map(cnt => cnt * biggestCoin).toList

            def countVariantsForCoin2(startingSums: List[Int]): Int = {
              val variants = if (startingSums.nonEmpty) countVariants(startingSums.head) else 0
              val otherVariants = if (startingSums.tail.nonEmpty) countVariantsForCoin2(startingSums.tail) else 0
              variants + otherVariants
            }

            def countVariants(sum: Int): Int = {
              val sideVariant = if (sum == money) 1 else 0
              val vars = if (descCoins.tail.nonEmpty) countChange(money - sum, descCoins.tail) else 0
              sideVariant + vars
            }

            countVariantsForCoin2(startingSums) + restChange
          }
      }
    }
  }
}
