package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean =
      if b then false
      else true

    def and(left: Boolean, right: => Boolean): Boolean =
      if not(left) then false
      else right

    def or(left: Boolean, right: => Boolean): Boolean =
      if left then true
      else right

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) =>
      @tailrec
      def mul(a: BigInt, b: BigInt, res: BigInt): BigInt =
        if a == 0 || b == 0 then 0
        else if b == 1 then res+a
        else mul(a, b-1, res+a)

      mul(a, b, res=0)

    val power: (BigInt, BigInt) => BigInt = (a,n) =>
      @tailrec
      def powerOfTheNumber(a: BigInt, n: BigInt, res: BigInt): BigInt =
        if n == 0 then res
        else if a == 0 then 0
        else powerOfTheNumber(a, n-1, multiplication(res, a))
      require(n >= 0, "Negative n is not allowed")
      val sign = if a < 0 && (n % 2 != 0) then -1 else 1
      sign * powerOfTheNumber(a.abs, n, res = 1)

    val fermatNumber: Int => BigInt = n =>
      require(n >= 0, "Negative n is not allowed")
      power(2, power(2, n))+1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :

    val lookAndSaySequenceElement: Int => BigInt = (n) =>

      val getHead: BigInt => BigInt = (num) =>
        num / `Fermat Numbers`.power(10, num.toString().length - 1)

      val getTail: BigInt => BigInt = (num) =>
        num % `Fermat Numbers`.power(10, num.toString().length - 1)

      val digits: BigInt => Int = (num: BigInt) =>
        num.toString().length

      val constructResult: (BigInt, Int, BigInt) => BigInt = (prev, count, result) =>
        result * `Fermat Numbers`.power(10, digits(count) + digits(prev)) + count * `Fermat Numbers`.power(10, digits(prev)) + prev

      @tailrec
      def nextElement(number: BigInt, prev: BigInt, count: Int, result: BigInt): BigInt =
        if (number == 0) then
          if (count > 0) then constructResult(prev, count, result)
          else result
        else
          val head = getHead(number)
          val tail = getTail(number)

          if (head == prev || prev == -1) then nextElement(tail, head, count + 1, result)
          else
            val newResult =
              if (prev != -1) then constructResult(prev, count, result)
              else result
            nextElement(tail, head, 1, newResult)

      def getLookAndSayElementN(n: Int): BigInt =
        @tailrec
        def lookAndSaySequence(current: BigInt, count: Int): BigInt =
          if n > count then
            val nextEl = nextElement(current, -1, 0, 0)
            lookAndSaySequence(nextEl, count + 1)
          else current

        require(n > 0, "Argument n must to be above zero.")
        lookAndSaySequence(BigInt(1), 1)

      getLookAndSayElementN(n)

  end `Look-and-say Sequence`

  object `Kolakoski sequence`:

    val kolakoski: Int => Int = (n) =>
      @tailrec
      def getElem(sequence: String, index: Int, first: Char, second: Char): Int =
        if index == n then
          if sequence.charAt(n - 1) == first then first.asDigit
          else second.asDigit
        else
          val newSubSequence =
            if sequence.charAt(index) == '2' then
              if sequence.last == '2' then s"$first$first"
              else s"$second$second"
            else
              if sequence.last == '2' then first.toString
              else second.toString
          val newSequence = sequence + newSubSequence
          getElem(newSequence, index + 1, first, second)

      require(n > 0, "The parameter n must be greater than zero.")
      val result = getElem("122", 2, '1', '2')
      result

  end `Kolakoski sequence`

end Homework
