package data.zipper

import org.scalatest.{FlatSpec, Matchers}

class ListZipperTest
    extends FlatSpec
    with Matchers {

  import ListZipper._

  "ListZipper" should "adjacent" in {
    val lz = fromList((1 to 10).toList).flatMap(_.findLeft(_ == 5)).get
    println(lz.allWithAdjacent(2))
  }

}
