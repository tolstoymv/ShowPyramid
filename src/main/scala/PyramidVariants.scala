

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

  def buildPyramidFold(length: Int, height: Int): String = {
    val initial = List.empty[String]
    (0 until height).foldLeft(initial) { case (acc, row) =>
      val nextLine = if ((2 * row) >= length) List.fill(length)('-')
      else List.fill(row)('-') ++ List.fill(length - 2 * row)('x') ++ List.fill(row)('-')
      nextLine.mkString :: acc

    }.mkString("\n")
  }

  def buildPyramidFoldAnotherSyntax(length: Int, height: Int): String = {
    val initial = List.empty[String]
    (0 until height).foldLeft(initial) {
      case (acc, row) if (2 * row) >= length => List.fill(length)('-').mkString :: acc
      case (acc, row) =>
        val formedLine = List.fill(row)('-') ++ List.fill(length - 2 * row)('x') ++ List.fill(row)('-')
        formedLine.mkString :: acc

    }.mkString("\n")
  }

}
