package karazin.scala.users.group.week1.homework

import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import Homework.*
import karazin.scala.users.group.week1.homework.arbitraries
import annotation.tailrec
import scala.util.Try

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    and(left, right) == (left && right)
  }

  property("eager and") = propBoolean {
    !and(left = false, throw new IllegalArgumentException("eager and is failed"))
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    or(left, right) == (left || right)
  }
  
  property("eager or") = propBoolean {
    or(left = true, throw new IllegalArgumentException("eager or is failed"))
  }
  
end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("Negative n is not allowed") = forAll { (a: Int, n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newN = if n != 0 then -Math.abs(n) else -1
      power(a, newN)
    }
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == BigInt(left).pow(right)
  }

  property("Negative n is not allowed") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newN = if n == 0 then -1 else -n
      fermatNumber(newN)
    }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(n) == BigInt(2).pow(BigInt(2).pow(n).toInt) + 1
  }

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  // https://rosettacode.org/wiki/Look-and-say_sequence#Scala
  @tailrec
  private def loop(n: Int, num: String): String = {
    if (n <= 0) num else loop(n - 1, lookAndSay(num))
  }

  private def lookAndSay(number: String): String = {
    val result = new StringBuilder

    @tailrec
    def loop(numberString: String, repeat: Char, times: Int): String =
      if (numberString.isEmpty) result.toString()
      else if (numberString.head != repeat) {
        result.append(times).append(repeat)
        loop(numberString.tail, numberString.head, 1)
      } else loop(numberString.tail, numberString.head, times + 1)

    loop(number.tail + " ", number.head, 1)
  }

  property("Argument n must to be above zero.") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val negativeOrZero = if n == 0 then 0 else -n
      lookAndSaySequenceElement(negativeOrZero)
    }
  }

  property("lookAndSaySequenceElement.") = forAll { (n: Int) =>
    (n > 0) ==> (lookAndSaySequenceElement(n).toString() == loop(n - 1, "1"))
  }

end LookAndAaSequenceSpecification

object KolakoskiSequence extends Properties("KolakoskiSequence"):
  import `Kolakoski sequence`._
  import arbitraries.given Arbitrary[Int]

  // https://rosettacode.org/wiki/Kolakoski_sequence#Kotlin
  // З посилання узято функцію для перевірки
  extension (arr: Array[Int])
    def nextInCycle(index: Int): Int = arr(index % arr.length)

    def kolakoskiElement(len: Int): Array[Int] = {
      val s = new Array[Int](len)
      var i = 0
      var k = 0
      while (true) {
        s(i) = arr.nextInCycle(k)
        if (s(k) > 1) {
          (1 until s(k)).foreach { _ =>
            i += 1
            if (i == len) return s
            s(i) = s(i - 1)
          }
        }
        i += 1
        if (i == len) return s
        k += 1
      }
      s
    }

  property("Throws exception for non-positive n") = forAll { (n: Int) =>
    val nonPositiveN = if (n <= 0) n else -n
    val result = Try(kolakoski(nonPositiveN))
    result.isFailure ==> result.failed.get.isInstanceOf[IllegalArgumentException]
  }

  val array = Array(1, 2)
  property("Valid last element from kolakoski and kolakoskiElement") = forAll { (n: Int) =>
    (n > 0) ==> {
      val kolakoskiLastElement = kolakoski(n)
      val iaLastElement = array.kolakoskiElement(n).last
      kolakoskiLastElement == iaLastElement
    }
  }

end KolakoskiSequence
