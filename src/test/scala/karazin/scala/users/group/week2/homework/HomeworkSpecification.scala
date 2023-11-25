package karazin.scala.users.group.week2.homework

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import utils._

object HomeworkSpecification extends Properties("Homework"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[Rational]}

  property("throw exception due to zero denominator") = forAll { (numer: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, -abs(kindaDenom))
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
    val resComDenom = left.denom * right.denom / gcd(left.denom, right.denom)
    val resRational = Rational((left.numer * (resComDenom / left.denom)) + (right.numer * (resComDenom / right.denom)),resComDenom)
    (left + right) == resRational
  }

  property("addition of Integer") = forAll { (left: Rational, int: Int) =>
    val newNumer = left.numer + left.denom * int
    val resRational = Rational(newNumer, left.denom)
    left + int == resRational
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    val resComDenom = left.denom * right.denom / gcd(left.denom, right.denom)
    val resRational = Rational((left.numer * (resComDenom / left.denom)) - (right.numer * (resComDenom / right.denom)), resComDenom)
    (left - right) == resRational
  }

  property("substraction of Integer") = forAll { (left: Rational, int: Int) =>
    val newNumer = left.numer - left.denom * int
    val resRational = Rational(newNumer, left.denom)
    left - int == resRational
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    val resRational = Rational((left.numer * right.numer), (left.denom * right.denom))
    left * right == resRational
  }

  property("multiplication of Integer") = forAll { (left: Rational, int: Int) =>
    val resRational = Rational(left.numer * int, left.denom)
    left * int == resRational
  }

  property("division") = forAll { (left: Rational, numer: Int, denom: Int) =>
    val right = Rational(if numer == 0 then 1 else numer, abs(denom) + 1)
    val newNumer = left.numer * right.denom
    val newDenom = right.numer * left.denom
    val resRational = if newDenom < 0 then Rational(-newNumer, -newDenom) else Rational(newNumer, newDenom)
    (left / right) == resRational
  }

  property("division of Integer") = forAll { (left: Rational, int: Int) =>
    val newDenom = left.denom * int
    val resRational = if newDenom < 0 then Rational(-left.numer, -newDenom) else Rational(left.numer, newDenom)
    left / int == resRational
  }

  property("division by zero") = forAll { (left: Rational, int: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val right = Rational(0)
      left / right
    }
  }

end HomeworkSpecification