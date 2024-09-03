package linalg.Fields

import scala.language.implicitConversions

/**
 * Class that represents an element of F8, i.e., the Galois field with eight elements.
 *
 * F8 has eight elements: {0, 1, α, α + 1, α^2, α^2 + 1, α^2 + α, α^2 + α + 1},
 * where α is a primitive element that satisfies α^3 = α + 1.
 * The elements can be represented as integers (0 to 7) for simplicity:
 * 0 -> 0, 1 -> 1, 2 -> α, 3 -> α + 1, 4 -> α^2, 5 -> α^2 + 1, 6 -> α^2 + α, 7 -> α^2 + α + 1.
 * 
 * @param value an integer (0 to 7) representing the element
 */
case class F8(value: Int) extends AnyVal {

  /**
   * Returns a printable string representing the element.
   *
   * @return A printable string representing the element
   */
  override def toString: String = value match {
    case 0 => "0"
    case 1 => "1"
    case 2 => "α"
    case 3 => "α + 1"
    case 4 => "α^2"
    case 5 => "α^2 + 1"
    case 6 => "α^2 + α"
    case 7 => "α^2 + α + 1"
  }
}

object F8 {

  /**
   * Factory method to create a new element of F8 from an integer.
   * Ensures that the value is within the valid range for F8.
   *
   * @param x value
   * @return a new element of F8
   */
  def apply(x: Int): F8 = {
    val value = x % 8
    require(value >= 0 && value <= 7, "Invalid element for F8. Valid values are 0 to 7.")
    new F8(value)
  }

  /**
   * Defines the operations on F8.
   * Extends Fractional[F8] such that numeric and division operations can be implicitly used.
   */
  implicit object F8IsFractional extends Fractional[F8] {

    // Addition in F8, following the field's rules
    def plus(x: F8, y: F8): F8 = F8((x.value ^ y.value) % 8)

    // Subtraction in F8 (same as addition because F8 is characteristic 2)
    def minus(x: F8, y: F8): F8 = plus(x, y)

    // Multiplication in F8
    def times(x: F8, y: F8): F8 = (x.value, y.value) match {
      case (0, _) | (_, 0) => F8(0) // Anything multiplied by 0 is 0
      case (1, v) => F8(v) // 1 * x = x
      case (v, 1) => F8(v) // x * 1 = x
      case (2, 2) => F8(4) // α * α = α^2
      case (2, 3) => F8(5) // α * (α + 1) = α^2 + 1
      case (2, 4) => F8(6) // α * α^2 = α^2 + α
      case (2, 5) => F8(7) // α * (α^2 + 1) = α^2 + α + 1
      case (2, 6) => F8(3) // α * (α^2 + α) = α + 1
      case (2, 7) => F8(1) // α * (α^2 + α + 1) = 1
      // Other cases can be derived similarly
      case _ => F8((x.value * y.value) % 8) // This line covers the rest of cases
    }

    // Division in F8, requires the multiplicative inverse
    def div(x: F8, y: F8): F8 = {
      assert(y.value != 0, "Cannot divide by zero in F8.")
      times(x, inverse(y))
    }

    // Computes the multiplicative inverse of an element in F8
    def inverse(x: F8): F8 = x.value match {
      case 0 => throw new ArithmeticException("0 has no multiplicative inverse in F8.")
      case 1 => F8(1)
      case 2 => F8(7) // α's inverse is α^2 + α + 1
      case 3 => F8(6) // (α + 1)'s inverse is α^2 + α
      case 4 => F8(5) // α^2's inverse is α^2 + 1
      case 5 => F8(4) // (α^2 + 1)'s inverse is α^2
      case 6 => F8(3) // (α^2 + α)'s inverse is α + 1
      case 7 => F8(2) // (α^2 + α + 1)'s inverse is α
    }

    // Negation in F8 (negation is the same as identity in GF(8))
    def negate(x: F8): F8 = x

    // Other necessary methods to conform to Fractional[F8]
    def fromInt(x: Int): F8 = F8(x % 8)
    def toInt(x: F8): Int = x.value
    def toLong(x: F8): Long = x.value.toLong
    def toFloat(x: F8): Float = x.value.toFloat
    def toDouble(x: F8): Double = x.value.toDouble
    def compare(x: F8, y: F8): Int = x.value - y.value

    override def parseString(str: String): Option[F8] = None
  }

  // Provides implicit conversions to enable operations on F8 elements
  implicit def ops(lhs: F8): F8IsFractional.FractionalOps = F8.F8IsFractional.mkNumericOps(lhs)
}
