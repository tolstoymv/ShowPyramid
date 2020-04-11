

object PyramidVariants {
  def buildPyramid(length: Int, height: Int): String = {
    def buildPyramidImpl(currentRow: Int, acc: List[List[Char]]): String = currentRow match {
      case num if num == height =>
        acc.map(_.mkString).mkString("\n")
      case _ =>
        val nextLine = if ((2 * currentRow) >= length) List.fill(length)('-')
        else List.fill(currentRow)('-') ++ List.fill(length - 2 * currentRow)('x') ++ List.fill(currentRow)('-')
        buildPyramidImpl(currentRow + 1, nextLine :: acc)
    }

    buildPyramidImpl(0, Nil)
  }

}
