import PyramidVariants._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PyramidSpec extends AnyFlatSpec with Matchers {
  "pyramid" should "fill one line if height is one" in {
    val expected = "xxxxx"
    buildPyramid(5, 1) should be(expected)
    buildPyramidFold(5, 1) should be(expected)
    buildPyramidFoldAnotherSyntax(5, 1) should be(expected)
  }

  it should "fill length and height" in {
    val expected =
      """--x--
        |-xxx-
        |xxxxx""".stripMargin

    buildPyramid(5, 3) should be(expected)
    buildPyramidFold(5, 3) should be(expected)
    buildPyramidFoldAnotherSyntax(5, 3) should be(expected)
  }

  it should "leave blank lines if height is too much" in {
    val expected =
      """-----
        |-----
        |--x--
        |-xxx-
        |xxxxx""".stripMargin

    buildPyramid(5, 5) should be(expected)
    buildPyramidFold(5, 5) should be(expected)
    buildPyramidFoldAnotherSyntax(5, 5) should be(expected)
  }

  it should "strip lines if height is not enough" in {
    val expected =
      """--xxx--
        |-xxxxx-
        |xxxxxxx""".stripMargin

    buildPyramid(7, 3) should be(expected)
    buildPyramidFold(7, 3) should be(expected)
    buildPyramidFoldAnotherSyntax(7, 3) should be(expected)
  }

  it should "handle even length" in {
    val expected =
      """----
        |-xx-
        |xxxx""".stripMargin

    buildPyramid(4, 3) should be(expected)
    buildPyramidFold(4, 3) should be(expected)
    buildPyramidFoldAnotherSyntax(4, 3) should be(expected)
  }
}
