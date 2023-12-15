package karazin.scala.users.group.week3

import scala.math.*
import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import Homework.*

import scala.annotation.tailrec


// Допоміжна опціональна функція fromInt, яка з типу Int зробить тип Nat
def fromInt(int: Int): Nat =
  def convertToNat(n: Int): Nat =
    if (n == 0) Zero
    else Succ(convertToNat(n - 1))

  require(int >= 0, "Number can't be negative")
  convertToNat(int)

// Допоміжна опціональна функція toInt, яка з типу Nat зробить тип Int
def toInt(nat: Nat): Int =
  @tailrec
  def convertToInt(n: Nat, count: Int): Int =
    if (n.isZero) count
    else convertToInt(n.predecessor, count + 1)

  convertToInt(nat, 0)

object HomeworkSpecification extends Properties("Homework"):

  include(ZeroSpecification)
  include(SuccSpecification)
  include(NatSpecification)

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):
  import arbitraries.{given Arbitrary[Zero], given Arbitrary[Nat]}

  property("Zero isZero") = forAll { (zero: Zero) =>
    zero.isZero
  }

  property("throw exception due to predecessor of Zero") = forAll { (zero: Zero) ⇒
    throws(classOf[Exception]) {
      zero.predecessor
    }
  }

  property("Zero plus Nat") = forAll { (zero: Zero, nat: Nat) =>
    zero + nat == nat
  }

  property("throw exception due to subtracting with zero") = forAll { (zero: Zero, nat: Nat) =>
    throws(classOf[IllegalArgumentException]) {
      val newNat = if nat.isZero then Succ(nat) else nat
      zero - newNat
    }
  }

  property("Zero toInt") = forAll { (zero: Zero) =>
    zero.toInt == 0
  }

  property("Zero equals") = forAll { (zeroFirst: Zero, zeroSec: Zero) =>
    (zeroFirst == zeroSec) && ((zeroFirst == Zero) == (zeroSec == Zero))
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):
  import arbitraries.{given Arbitrary[Succ], given Arbitrary[Nat], given Arbitrary[Zero]}

  property("Succ isZero") = forAll { (succ: Succ) =>
    !succ.isZero
  }

  property("predecessor of Succ") = forAll { (succ: Succ) =>
    succ.predecessor == (succ - Succ(Zero))
  }

  property("Succ plus Nat") = forAll { (succ: Succ, nat: Nat) =>
    val result = if nat.isZero then succ else Succ(succ) + nat.predecessor
    succ + nat == result
  }

  property("Succ minus Nat, throw exception if Nat is greater than Succ") = forAll { (succ: Succ, nat: Nat) =>
    if toInt(succ) >= toInt(nat) then succ - nat == fromInt(toInt(succ) - toInt(nat))
    else throws(classOf[IllegalArgumentException]) {
      succ - nat == fromInt(toInt(succ) - toInt(nat))
    }
  }

  property("Succ to Int") = forAll { (succ: Succ) =>
    succ.toInt == toInt(succ)
  }

  property("Succ equals") = forAll { (succLeft: Succ, succRight: Succ) =>
    (succLeft == succRight) == (toInt(succLeft) == toInt(succRight))
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  import arbitraries.{given Arbitrary[Succ], given Arbitrary[Nat], given Arbitrary[Zero]}

  property("Nat isZero") = forAll { (nat: Nat) =>
    if nat.isInstanceOf[Succ] then !nat.isZero
    else nat.isZero
  }

  property("predecessor of Nat") = forAll { (nat: Nat) =>
    if nat.isInstanceOf[Succ] then nat.predecessor == (nat - Succ(Zero))
    else throws(classOf[Exception]) {
         nat.predecessor
    }
  }

  property("successor of Nat") = forAll { (nat: Nat) =>
    nat.successor == Succ(nat)
  }

  property("Nat plus Nat") = forAll { (natLeft: Nat, natRight: Nat) =>
    (natLeft + natRight) == fromInt(toInt(natLeft) + toInt(natRight))
  }

  property("Nat minus Nat, throw exception if natRight is greater than natLeft") = forAll { (natLeft: Nat, natRight: Nat) =>
    if toInt(natLeft) >= toInt(natRight) then natLeft - natRight == fromInt(toInt(natLeft) - toInt(natRight))
    else throws(classOf[IllegalArgumentException]) {
      natLeft - natRight == fromInt(toInt(natLeft) - toInt(natRight))
    }
  }

  property("Nat to Int") = forAll { (nat: Nat) =>
    nat.toInt == toInt(nat)
  }
  
  property("Nat equals") = forAll { (natLeft: Nat, natRight: Nat) =>
    (natLeft == natRight) == (toInt(natLeft) == toInt(natRight))
  }

end NatSpecification
  