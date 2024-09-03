package linalg.Fields

import scala.language.implicitConversions

/**
 * Class that represents an element of F4, i.e., the Galois field with four elements.
 * 
 * F4 has four elements: {0, 1, α, α + 1}, where α is a primitive element that satisfies α^2 = α + 1.
 * The elements can be represented as integers (0, 1, 2, 3) for simplicity:
 * 0 -> 0, 1 -> 1, 2 -> α, 3 -> α + 1.
 * 
 * @param value an integer (0, 1, 2, or 3) representing the element
 */
case class F4(value: Int) extends AnyVal {

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
  }
}

object F4 {

  /**
   * Factory method to create a new element of F4 from an integer.
   * Ensures that the value is within the valid range for F4.
   *
   * @param x value
   * @return a new element of F4
   */
  def apply(x: Int): F4 = {
    val value = x % 4
    require(value >= 0 && value <= 3, "Invalid element for F4. Valid values are 0, 1, 2, 3.")
    new F4(value)
  }

  /**
   * Defines the operations on F4.
   * Extends Fractional[F4] such that numeric and division operations can be implicitly used.
   */
  implicit object F4IsFractional extends Fractional[F4] {
    
    // Addition in F4, following the field's rules
    def plus(x: F4, y: F4): F4 = F4((x.value ^ y.value) % 4)

    // Subtraction in F4 (same as addition because F4 is characteristic 2)
    def minus(x: F4, y: F4): F4 = plus(x, y)  // same as plus in GF(4)

    // Multiplication in F4
    def times(x: F4, y: F4): F4 = (x.value, y.value) match {
      case (0, _) | (_, 0) => F4(0) // Anything multiplied by 0 is 0
      case (1, v) => F4(v) // 1 * x = x
      case (v, 1) => F4(v) // x * 1 = x
      case (2, 2) => F4(3) // α * α = α + 1
      case (2, 3) => F4(1) // α * (α + 1) = 1
      case (3, 2) => F4(1) // (α + 1) * α = 1
      case (3, 3) => F4(2) // (α + 1) * (α + 1) = α
    }

    // Division in F4, requires the multiplicative inverse
    def div(x: F4, y: F4): F4 = {
      assert(y.value != 0, "Cannot divide by zero in F4.")
      times(x, inverse(y))
    }

    // Computes the multiplicative inverse of an element in F4
    def inverse(x: F4): F4 = x.value match {
      case 0 => throw new ArithmeticException("0 has no multiplicative inverse in F4.")
      case 1 => F4(1)
      case 2 => F4(3) // α's inverse is α + 1
      case 3 => F4(2) // (α + 1)'s inverse is α
    }

    // Negation in F4 (negation is the same as identity in GF(4))
    def negate(x: F4): F4 = x

    // Other necessary methods to conform to Fractional[F4]
    def fromInt(x: Int): F4 = F4(x % 4)
    def toInt(x: F4): Int = x.value
    def toLong(x: F4): Long = x.value.toLong
    def toFloat(x: F4): Float = x.value.toFloat
    def toDouble(x: F4): Double = x.value.toDouble
    def compare(x: F4, y: F4): Int = x.value - y.value

    override def parseString(str: String): Option[F4] = None
  }

  // Provides implicit conversions to enable operations on F4 elements
  implicit def ops(lhs: F4): F4IsFractional.FractionalOps = F4.F4IsFractional.mkNumericOps(lhs)
}
