package karazin.scala.users.group.week4.homework

import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import arbitraries.given
import Homework._

// Допоміжна функція перетворення IntSet в Set[Int]
def toSet(set: IntSet): Set[Int] =
  if set.isInstanceOf[NonEmpty] then
    val nonEmpty = set.asInstanceOf[NonEmpty]
    Set(nonEmpty.elem) ++ toSet(nonEmpty.left) ++ toSet(nonEmpty.right)
  else Set.empty[Int]

object HomeworkSpecification extends Properties("Homework"):

  include(EmptySpecification)
  include(NonEmptySpecification)
  include(IntSetSpecification)

end HomeworkSpecification

// Add additional cases if needed
object EmptySpecification extends Properties("Empty"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[NonEmpty], given Arbitrary[IntSet]}

  property("equals to Empty") = propBoolean {
    Empty == Empty
  }

  property("not equal to NonEmpty") = forAll { (nonEmpty: NonEmpty) ⇒
    Empty != nonEmpty
  }

  property("include") = forAll { (element: Int) ⇒
    (Empty include element) == NonEmpty(element, Empty, Empty)
  }

  property("contains") = forAll { (element: Int) ⇒
    !(Empty contains element)
  }

  property("remove") = forAll { (element: Int) ⇒
    throws(classOf[Exception]) {
      (Empty remove element) == Empty
    }
  }

  property("union") = forAll { (set: IntSet) ⇒
    (Empty ∪ set) == set
  }

  property("intersection") = forAll { (set: IntSet) ⇒
    (Empty ∩ set) == Empty
  }

  property("complement of Empty") = forAll { (set: IntSet) ⇒
    (set ∖ Empty) == set
  }

  property("complement of set") = forAll { (set: IntSet) ⇒
    (Empty ∖ set) == Empty
  }

  property("left disjunctive union") = forAll { (set: IntSet) ⇒
    (Empty ∆ set) == set
  }

  property("right disjunctive union") = forAll { (set: IntSet) ⇒
    (set ∆ Empty) == set
  }

end EmptySpecification

// Add additional cases if needed
object NonEmptySpecification extends Properties("NonEmpty"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[NonEmpty], given Arbitrary[IntSet]}

  property("not equals to Empty") = forAll { (nonEmpty: NonEmpty) ⇒
    nonEmpty != Empty
  }

  property("equal") = forAll { (nonEmpty: NonEmpty) ⇒
    nonEmpty == nonEmpty
  }

  property("include") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    val setNonEmpty = toSet(nonEmpty)
    val newNonEmpty = setNonEmpty + element
    toSet(nonEmpty include element) == newNonEmpty
  }

  property("contains") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    val setNonEmpty = toSet(nonEmpty)
    val answer = setNonEmpty(element)
    (nonEmpty contains element) == answer
  }

  property("remove") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    val setNonEmpty = toSet(nonEmpty)
    if nonEmpty contains element then
      val newNonEmpty = setNonEmpty - element
      toSet(nonEmpty remove element) == newNonEmpty
    else
      throws(classOf[Exception]) {
        val newNonEmpty = setNonEmpty - element
        toSet(nonEmpty remove element) == newNonEmpty
      }
  }

  property("union") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    val setNonEmpty = toSet(nonEmpty)
    val setSet = toSet(set)
    val setResult = toSet(nonEmpty ∪ set)

    setResult == (setNonEmpty | setSet)
  }

  property("intersection") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    val setNonEmpty = toSet(nonEmpty)
    val setSet = toSet(set)
    val setResult = toSet(nonEmpty ∩ set)

    setResult == (setNonEmpty & setSet)
  }

  property("complement") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    val setNonEmpty = toSet(nonEmpty)
    val setSet = toSet(set)
    val setResult = toSet(nonEmpty ∖ set)

    setResult == (setNonEmpty &~ setSet)
  }

  property("disjunctive") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    val setNonEmpty = toSet(nonEmpty)
    val setSet = toSet(set)
    val setResult = toSet(nonEmpty ∆ set)

    setResult == ((setNonEmpty &~ setSet) | (setSet &~ setNonEmpty))
  }

end NonEmptySpecification

// Add additional cases if needed
object IntSetSpecification extends Properties("IntSet"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[IntSet]}

  property("equals") = forAll { (set: IntSet) ⇒
    set == set
  }

end IntSetSpecification