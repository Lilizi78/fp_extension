package linalg.Fields

import scala.language.implicitConversions

/**
 * Class that represents an element of F16, i.e., the Galois field with sixteen elements.
 *
 * F16 has sixteen elements: {0, 1, α, α^2, ..., α^14}, where α is a primitive element that satisfies α^4 + α + 1 = 0.
 * The elements can be represented as integers (0 to 15) for simplicity.
 *
 * @param value an integer (0 to 15) representing the element
 */
case class F16(value: Int) extends AnyVal {

  /**
   * Returns a printable string representing the element.
   *
   * @return A printable string representing the element
   */
  override def toString: String = value match {
    case 0 => "0"
    case 1 => "1"
    case 2 => "α"
    case 3 => "α^2"
    case 4 => "α^3"
    case 5 => "α^4"
    case 6 => "α^5"
    case 7 => "α^6"
    case 8 => "α^7"
    case 9 => "α^8"
    case 10 => "α^9"
    case 11 => "α^10"
    case 12 => "α^11"
    case 13 => "α^12"
    case 14 => "α^13"
    case 15 => "α^14"
  }
}

object F16 {

  /**
   * Factory method to create a new element of F16 from an integer.
   * Ensures that the value is within the valid range for F16.
   *
   * @param x value
   * @return a new element of F16
   */
  def apply(x: Int): F16 = {
    val value = x % 16
    require(value >= 0 && value <= 15, "Invalid element for F16. Valid values are 0 to 15.")
    new F16(value)
  }

  /**
   * Defines the operations on F16.
   * Extends Fractional[F16] such that numeric and division operations can be implicitly used.
   */
  implicit object F16IsFractional extends Fractional[F16] {

    // Addition in F16, following the field's rules (XOR operation)
    def plus(x: F16, y: F16): F16 = F16((x.value ^ y.value) % 16)

    // Subtraction in F16 (same as addition because F16 is characteristic 2)
    def minus(x: F16, y: F16): F16 = plus(x, y)

    // Multiplication in F16, use precomputed logarithm table
    def times(x: F16, y: F16): F16 = (x.value, y.value) match {
      case (0, _) | (_, 0) => F16(0)  // Anything multiplied by 0 is 0
      case (a, b) => F16(mulTable(a)(b))
    }

    // Division in F16, requires the multiplicative inverse
    def div(x: F16, y: F16): F16 = {
      assert(y.value != 0, "Cannot divide by zero in F16.")
      times(x, inverse(y))
    }

    // Computes the multiplicative inverse of an element in F16
    def inverse(x: F16): F16 = {
      assert(x.value != 0, "0 has no multiplicative inverse in F16.")
      F16(invTable(x.value))
    }

    // Negation in F16 (negation is the same as identity in GF(16))
    def negate(x: F16): F16 = x

    // Other necessary methods to conform to Fractional[F16]
    def fromInt(x: Int): F16 = F16(x % 16)
    def toInt(x: F16): Int = x.value
    def toLong(x: F16): Long = x.value.toLong
    def toFloat(x: F16): Float = x.value.toFloat
    def toDouble(x: F16): Double = x.value.toDouble
    def compare(x: F16, y: F16): Int = x.value - y.value

    override def parseString(str: String): Option[F16] = None
  }

  // Precomputed multiplication table for F16
  private val mulTable: Array[Array[Int]] = Array.ofDim[Int](16, 16)
  // Precomputed inverse table for F16
  private val invTable: Array[Int] = Array(0, 1, 9, 14, 13, 11, 15, 10, 7, 2, 6, 5, 8, 4, 3, 12)


  // Initialize multiplication and inverse tables
  initializeTables()

  private def initializeTables(): Unit = {
    // Multiplication rules based on the primitive polynomial x^4 + x + 1 over GF(2)
    // Fill in the table according to the multiplication rules in F16

    for (i <- 0 until 16) {
      for (j <- 0 until 16) {
        if (i == 0 || j == 0) {
          mulTable(i)(j) = 0 // 0 multiplied by anything is 0
        } else {
          // Multiplication using the exponentiation and modulo reduction with the irreducible polynomial
          val result = (logTable(i) + logTable(j)) % 15
          mulTable(i)(j) = expTable(result + 1)
        }
      }
    }

    // Initialize inverse table
    invTable(1) = 1   // 1's inverse is 1
    for (i <- 2 until 16) {
      invTable(expTable(15 - logTable(i))) = i
    }
  }

  private val logTable: Array[Int] = Array(
    -1, 0, 1, 4, 2, 8, 5, 10, 3, 14, 9, 7, 6, 12, 11, 13
  )
  private val expTable: Array[Int] = Array(
    1, 2, 4, 8, 3, 6, 12, 11, 5, 10, 7, 14, 15, 9, 13
  )

  // Provides implicit conversions to enable operations on F16 elements
  implicit def ops(lhs: F16): F16IsFractional.FractionalOps = F16.F16IsFractional.mkNumericOps(lhs)
}
