package day3

import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day3Test extends AnyFunSuiteLike with Matchers {
  test("test part 1") {
    Day3.part1("src/test/scala/day3/part1Test").unsafeRunSync() shouldBe 4361
  }

  test("code part 1") {
    Day3.part1("src/test/scala/day3/part1").unsafeRunSync() shouldBe 532428
  }
  test("test part 2") {
    Day3.part2("src/test/scala/day3/part1Test").unsafeRunSync() shouldBe 467835
  }

  test("code part 2") {
    Day3.part2("src/test/scala/day3/part1").unsafeRunSync() shouldBe 84051670
  }
}
