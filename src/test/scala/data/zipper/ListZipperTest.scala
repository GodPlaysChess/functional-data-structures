package data.zipper

import org.scalatest.{ FlatSpec, Matchers }

class ListZipperTest extends FlatSpec with Matchers {

  import ListZipper._

  "ListZipper" should "adjacent" in {
    val lz0 = fromList((1 to 10).toList)
    println(lz0)
    val lz1 = lz0.flatMap(_.findRight(_ == 5)).get
    println(lz1.allWithAdjacent(2))
  }
}
