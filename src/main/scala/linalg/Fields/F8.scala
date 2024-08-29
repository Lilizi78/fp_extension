package linalg.Fields

import scala.language.implicitConversions

/**
 * Class that represent an element of F8, i.e. the Galois field with eight elements
 *
 * @param value an integer (0 to 7) representing the element
 */
case class F8(value: Int) extends AnyVal {
  /**
   * Returns a printable string representing the number
   *
   * @return A printable string representing the number
   */
  override def toString: String = value.toString
}

object F8 {
  /**
   * Creates a new element of F8 from an integer
   *
   * @param x value
   * @return a new element of F8
   */
  implicit def fromInt(x: Int): F8 = F8IsFractional.fromInt(x)

  /**
   * Defines the operations on F8
   * Extends Fractional[F8] such that numeric and division operations can be implicitly used
   */
  implicit object F8IsFractional extends Fractional[F8] {
    def div(x: F8, y: F8): F8 = {
      assert(y.value != 0)
      F8(x.value / y.value)
    }

    def toDouble(x: F8): Double = x.value.toDouble

    def toFloat(x: F8): Float = x.value.toFloat

    def negate(x: F8): F8 = F8(-x.value % 8)

    def fromInt(x: Int): F8 = F8(x % 8)

    def toLong(x: F8): Long = x.value.toLong

    def times(x: F8, y: F8): F8 = F8((x.value * y.value) % 8)

    def minus(x: F8, y: F8): F8 = F8((x.value - y.value) % 8)

    def plus(x: F8, y: F8): F8 = F8((x.value + y.value) % 8)

    def compare(x: F8, y: F8): Int = x.value - y.value

    def toInt(x: F8): Int = x.value

    override def parseString(str: String): Option[F8] = ???
  }

  implicit def ops(lhs: F8): F8IsFractional.FractionalOps = F8.F8IsFractional.mkNumericOps(lhs)
}
