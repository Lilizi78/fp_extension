package linalg.Fields

import scala.language.implicitConversions

/**
 * Class that represent an element of F4, i.e. the Galois field with four elements
 *
 * @param value an integer (0, 1, 2, or 3) representing the element
 */
case class F4(value: Int) extends AnyVal {
  /**
   * Returns a printable string representing the number
   *
   * @return A printable string representing the number
   */
  override def toString: String = value.toString
}

object F4 {
  /**
   * Creates a new element of F4 from an integer
   *
   * @param x value
   * @return a new element of F4
   */
  implicit def fromInt(x: Int): F4 = F4IsFractional.fromInt(x)

  /**
   * Defines the operations on F4
   * Extends Fractional[F4] such that numeric and division operations can be implicitly used
   */
  implicit object F4IsFractional extends Fractional[F4] {
    def div(x: F4, y: F4): F4 = {
      assert(y.value != 0)
      F4(x.value / y.value)
    }

    def toDouble(x: F4): Double = x.value.toDouble

    def toFloat(x: F4): Float = x.value.toFloat

    def negate(x: F4): F4 = F4(-x.value % 4)

    def fromInt(x: Int): F4 = F4(x % 4)

    def toLong(x: F4): Long = x.value.toLong

    def times(x: F4, y: F4): F4 = F4((x.value * y.value) % 4)

    def minus(x: F4, y: F4): F4 = F4((x.value - y.value) % 4)

    def plus(x: F4, y: F4): F4 = F4((x.value + y.value) % 4)

    def compare(x: F4, y: F4): Int = x.value - y.value

    def toInt(x: F4): Int = x.value

    override def parseString(str: String): Option[F4] = ???
  }

  implicit def ops(lhs: F4): F4IsFractional.FractionalOps = F4.F4IsFractional.mkNumericOps(lhs)
}
