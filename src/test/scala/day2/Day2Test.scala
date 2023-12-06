package day2

import cats.effect.unsafe.implicits.global
import day2.Day2.Bag
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day2Test extends AnyFunSuiteLike with Matchers {
  private val bag = Bag(red = 12, green = 13, blue = 14)
  test("test part 1") {
    Day2.part1("src/test/scala/day2/part1Test", bag).unsafeRunSync() shouldBe 8
  }

  test("code part 1") {
    Day2.part1("src/test/scala/day2/part1", bag).unsafeRunSync() shouldBe 2771
  }
  test("test part 2") {
    Day2.part2("src/test/scala/day2/part1Test").unsafeRunSync() shouldBe 2286
  }

  test("code part 2") {
    Day2.part2("src/test/scala/day2/part1").unsafeRunSync() shouldBe 70924
  }
}
