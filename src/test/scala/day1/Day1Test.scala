package day1

import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Day1Test extends AnyFunSuiteLike with Matchers {
  test("test part 1") {
    Day1.part1("src/test/scala/day1/part1Test").unsafeRunSync() shouldBe 142
  }

  test("code part 1") {
    Day1.part1("src/test/scala/day1/part1").unsafeRunSync() shouldBe 55017
  }
  test("test part 2") {
    Day1.part2("src/test/scala/day1/part2Test").unsafeRunSync() shouldBe 281
  }

  test("code part 2") {
    Day1.part2("src/test/scala/day1/part1").unsafeRunSync() shouldBe 53539
  }
}
