package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = Succ(this)
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
     // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int): Nat =
      def convertToNat(n: Int): Nat =
        if (n == 0) Zero
        else Succ(convertToNat(n - 1))

      require(int >= 0, "Number can't be negative")
      convertToNat(int)

    override def toString: String = s"Succ($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat =
      if !that.isZero then throw IllegalArgumentException("You can't get a negative subtractional result.")
      else this
    
    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean =
      if obj.isInstanceOf[Zero] then
        val that = obj.asInstanceOf[Zero]
        this.isZero && that.isZero
      else false

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n

    infix def +(that: Nat): Nat =
      if that.isZero then this
      else Succ(this) + that.predecessor
    
    infix def -(that: Nat): Nat =
      if that.isZero then this
      else if this.isZero then Zero
      else this.predecessor - that.predecessor

    // Optional task
    def toInt: Int =
      @tailrec
      def convertToInt(n: Nat, count: Int): Int =
        if (n.isZero) count
        else convertToInt(n.predecessor, count + 1)

      convertToInt(this, 0)

    override def equals(obj: Any): Boolean =
      if obj.isInstanceOf[Succ] then
        val that = obj.asInstanceOf[Succ]
        (this.predecessor == that.predecessor) && (this.isZero == that.isZero)
      else false