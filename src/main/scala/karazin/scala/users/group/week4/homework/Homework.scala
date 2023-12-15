package karazin.scala.users.group.week4.homework

import scala.annotation.targetName
import karazin.scala.users.group.week4.utils.ItemOrdering

object Homework:

  abstract class IntSet:

    infix def include(x: Int): IntSet

    infix def remove(x: Int): IntSet

    infix def contains(x: Int): Boolean

    @targetName("union")
    infix def ∪(that: IntSet): IntSet

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet

  end IntSet

  type Empty = Empty.type
  
  case object Empty extends IntSet:
    
    infix def include(x: Int): IntSet = NonEmpty(x, Empty, Empty)

    infix def contains(x: Int): Boolean = false

    infix def remove(x: Int): IntSet = throw new Exception("You cannot remove an element from an Empty")
    
    @targetName("union")
    infix def ∪(that: IntSet): IntSet = that

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = Empty

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet = Empty

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet = that
    
    override def toString: String = "[*]"    
    
    override def equals(other: Any): Boolean =
      if other.isInstanceOf[Empty] then true
      else false

    override def hashCode(): Int = 0

  end Empty
    
  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:

    infix def include(x: Int): IntSet = 
      if x < elem       then NonEmpty(elem, left include x, right)
      else if x > elem  then NonEmpty(elem, left, right include x)
      else              this

    infix def contains(x: Int): Boolean = 
      if x < elem       then left contains x
      else if x > elem  then right contains x
      else              true

    // Optional task
    infix def remove(x: Int): IntSet =
      if this contains x then
        if x == elem then left ∪ right
        else if x < elem then NonEmpty(elem, left.remove(x), right)
        else NonEmpty(elem, left, right.remove(x))
      else throw new Exception("The element is not included in NonEmpty")

    private def maxValueAndRemove(set: IntSet): (Int, IntSet) =
      set match
      case nonEmpty: NonEmpty =>
        if nonEmpty.right == Empty then
          (nonEmpty.elem, nonEmpty.left)
        else maxValueAndRemove(right)
      case Empty => throw new Exception("Found Empty set error")

    @targetName("union")
    infix def ∪(that: IntSet): IntSet =
      that match
        case thatNonEmpty: NonEmpty =>
          (this include thatNonEmpty.elem) ∪ thatNonEmpty.left ∪ thatNonEmpty.right
        case _ => this

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet =
      that match
        case thatNonEmpty: NonEmpty =>
          if this contains thatNonEmpty.elem then NonEmpty(thatNonEmpty.elem, this ∩ thatNonEmpty.left, this ∩ thatNonEmpty.right)
          else (this ∩ thatNonEmpty.left) ∪ (this ∩ thatNonEmpty.right)
        case _ => Empty

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet =
      that match
        case thatNonEmpty: NonEmpty =>
          if thatNonEmpty contains this.elem then (this.left ∖ thatNonEmpty) ∪ (this.right ∖ thatNonEmpty)
          else NonEmpty(this.elem, this.left ∖ thatNonEmpty, this.right ∖ thatNonEmpty)
        case _ => this

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet =
      that match
        case thatEmpty: Empty => this
        case _ => (this ∖ that) ∪ (that ∖ this)
    
    override def toString: String = s"[$left - [$elem] - $right]"    
    override def equals(other: Any): Boolean =
      if other.isInstanceOf[NonEmpty] then
        val that = other.asInstanceOf[NonEmpty]
        if this.hashCode != that.hashCode then false
        else
          (this.elem == that.elem) && (this.left == that.left) && (this.right == that.right)
      else false

    override def hashCode(): Int = {
      val state = Seq(elem.hashCode, left.hashCode, right.hashCode)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

  end NonEmpty

end Homework
