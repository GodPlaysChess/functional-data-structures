package data.basic

import org.scalatest.{ FlatSpec, Matchers }
import List._
import ListFixture._

class ListTest extends FlatSpec with Matchers {
  behavior of "List"

  it should "reverse" in {
    OneToFive.reverse shouldBe List.fromScala((1 to 5).reverse.toList)
  }

  it should "append correctly" in {
    OneToFive ++ FiveToTen shouldBe List.fromScala((1 to 10).toList)
  }

}

object ListFixture {
  val OneToFive = 1 :: 2 :: 3 :: 4 :: 5 :: nil
  val FiveToTen = 6 :: 7 :: 8 :: 9 :: 10 :: nil
}
