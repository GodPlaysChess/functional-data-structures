package data.zipper

import data.zipper.ListZipper.fromList
import org.scalatest.{ FlatSpec, Matchers }
import ListZipperTestFixture._

class ListZipperTest extends FlatSpec with Matchers {

  import ListZipper._

  "ListZipper" should "adjacent" in {
    centeredFive.allWithAdjacent(2) should have size 2
  }

  it should "duplicate" in {
    println(centeredFive.duplicate)
  }

}

object ListZipperTestFixture {
  val centeredFive: ListZipper[Int] = fromList((1 to 10).toList).flatMap(_.findRight(_ == 5)).get
}
