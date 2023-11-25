package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")  // 1/2 + 3/5; 1/2 + 2 = 3/2; (Добавить для инта каждому методу)
    infix def +(that: Rational): Rational =
      val comDenom = this.denom * that.denom / gcd(this.denom, that.denom)
      val newNumer = (this.numer * (comDenom / this.denom)) + (that.numer * (comDenom / that.denom))
      Rational(newNumer, comDenom)

    @targetName("addition of Integer")
    infix def +(that: Int): Rational =
      val newNumer = this.numer + this.denom * that
      Rational(newNumer, this.denom)

    @targetName("negation")
    infix def unary_- : Rational =
      val newNumer = this.numer * -1
      Rational(newNumer, this.denom)

    @targetName("substraction")
    infix def -(that: Rational): Rational =
      val comDenom = this.denom * that.denom / gcd(this.denom, that.denom)
      val newNumer = (this.numer * (comDenom / this.denom)) - (that.numer * (comDenom / that.denom))
      Rational(newNumer, comDenom)

    @targetName("substraction of Integer")
    infix def -(that: Int): Rational =
      val newNumer = this.numer - this.denom * that
      Rational(newNumer, this.denom)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      val newDenom = this.denom * that.denom
      val newNumer = this.numer * that.numer
      Rational(newNumer, newDenom)

    @targetName("multiplication of Integer")
    infix def *(that: Int): Rational =
      val newNumer = this.numer * that
      Rational(newNumer, this.denom)

    @targetName("devision")
    infix def /(that: Rational): Rational =
      val newNumer = this.numer * that.denom
      val newDenom = that.numer * this.denom
      if newDenom < 0 then Rational(-newNumer, -newDenom) else Rational(newNumer, newDenom)

    @targetName("devision of Integer")
    infix def /(that: Int): Rational =
      val newDenom = this.denom * that
      if newDenom < 0 then Rational(-this.numer, -newDenom) else Rational(this.numer, newDenom)

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)
    override def equals(other: Any): Boolean =
      if other.isInstanceOf[Rational] then
        val that = other.asInstanceOf[Rational]
        this.numer == that.numer && this.denom == that.denom
      else false

  end Rational
end Homework
