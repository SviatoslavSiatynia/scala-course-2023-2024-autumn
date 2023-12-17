package karazin.scala.users.group.week2.homework

import scala.language.implicitConversions
import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import karazin.scala.users.group.week2.arbitraries.restricted.{Integer, Zero, PositiveInteger, NegativeInteger}
import utils._

object HomeworkSpecification extends Properties("Homework"):

  import arbitraries.{
    given Arbitrary[Int], given Arbitrary[Rational], given Arbitrary[NegativeInteger],
    given Arbitrary[Integer], given Arbitrary[Zero], given Arbitrary[PositiveInteger]
  }

  property("throw exception due to zero denominator") = forAll { (numer: Int, zero: Zero) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, zero)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, negativeInt: NegativeInteger) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, negativeInt)
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Int, int: Int) ⇒
    val denom = abs(int) + 1
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == ( left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == ( left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    -rational == (rational * -1)
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    val resComDenom = left.denom * right.denom
    val resRational = Rational((left.numer * (resComDenom / left.denom)) + (right.numer * (resComDenom / right.denom)),resComDenom)
    (left + right) == resRational
  }

  property("addition of Integer") = forAll { (left: Rational, int: Integer) =>
    val right = Rational(int, 1)
    (left + int) == (left + right)
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    val resComDenom = left.denom * right.denom
    val resRational = Rational((left.numer * (resComDenom / left.denom)) - (right.numer * (resComDenom / right.denom)), resComDenom)
    (left - right) == resRational
  }

  property("substraction of Integer") = forAll { (left: Rational, int: Integer) =>
    val right = Rational(int, 1)
    (left - int) == (left - right)
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    val resRational = Rational((left.numer * right.numer), (left.denom * right.denom))
    left * right == resRational
  }

  property("multiplication of Integer") = forAll { (left: Rational, int: Integer) =>
    val right = Rational(int, 1)
    (left * int) == (left * right)
  }

  property("division") = forAll { (left: Rational, numer: Integer, denom: Integer) =>
    val right = Rational(if numer equals 0 then 1 else numer, abs(denom) + 1)
    val newNumer = left.numer * right.denom
    val newDenom = right.numer * left.denom
    val resRational = if newDenom < 0 then Rational(-newNumer, -newDenom) else Rational(newNumer, newDenom)
    (left / right) == resRational
  }

  property("division of NegativeInteger") = forAll { (left: Rational, int: NegativeInteger) =>
    val right = Rational(int, 1)
    (left / int) == (left / right)
  }

  property("division of PositiveInteger") = forAll { (left: Rational, int: PositiveInteger) =>
    val right = Rational(int, 1)
    (left / int) == (left / right)
  }

  property("division by zero") = forAll { (left: Rational, zero: Zero) =>
    throws(classOf[IllegalArgumentException]) {
      val right = Rational(zero)
      left / right
    }
  }

  property("addition Int of Rational") = forAll { (int: Integer, right: Rational) =>
    val left = Rational(int, 1)
    (int + right) == (left + right)
  }

  property("subtraction Int of Rational") = forAll { (int: Integer, right: Rational) =>
    val left = Rational(int, 1)
    (int - right) == (left - right)
  }

  property("multiplication Int of Rational") = forAll { (int: Integer, right: Rational) =>
    val left = Rational(int, 1)
    (int * right) == (left * right)
  }

  property("division PositiveInteger of Rational") = forAll { (int: PositiveInteger, right: Rational) =>
    val left = Rational(int, 1)
    (int / right) == (left / right)
  }

  property("division NegativeInteger of Rational") = forAll { (int: NegativeInteger, right: Rational) =>
    val left = Rational(int, 1)
    (int / right) == (left / right)
  }

  property("division Zero of Rational") = forAll { (zero: Zero, right: Rational) =>
    val left = Rational(zero, 1)
    val newRight = if right.numer == 0 then Rational(1, right.denom) else right
    (zero / newRight) == (left / newRight)
  }

end HomeworkSpecification