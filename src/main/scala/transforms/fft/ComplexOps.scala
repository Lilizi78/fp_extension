package transforms.fft

import linalg.Fields.Complex

/**
 * A type class that extends Numeric[T] and provides additional operations for complex numbers.
 * 
 * @tparam T The numeric type.
 */
trait ComplexOps[T] extends Numeric[T] {
  def times(x: T, y: T): T  // Define a times method to support complex multiplication.
}

object ComplexOps {
  // Helper method to get an implicit instance
  def apply[T](implicit ev: ComplexOps[T]): ComplexOps[T] = ev

  // Provide an implicit instance for Complex[Double]
  implicit object ComplexDoubleOps extends ComplexOps[Complex[Double]] {
    // Use the given Fractional instance for complex numbers
    private val complexFrac = summon[Fractional[Complex[Double]]]

    override def plus(x: Complex[Double], y: Complex[Double]): Complex[Double] = complexFrac.plus(x, y)
    override def minus(x: Complex[Double], y: Complex[Double]): Complex[Double] = complexFrac.minus(x, y)
    override def times(x: Complex[Double], y: Complex[Double]): Complex[Double] = complexFrac.times(x, y)
    override def negate(x: Complex[Double]): Complex[Double] = complexFrac.negate(x)
    override def fromInt(x: Int): Complex[Double] = complexFrac.fromInt(x)
    override def toInt(x: Complex[Double]): Int = complexFrac.toInt(x)
    override def toLong(x: Complex[Double]): Long = complexFrac.toLong(x)
    override def toFloat(x: Complex[Double]): Float = complexFrac.toFloat(x)
    override def toDouble(x: Complex[Double]): Double = complexFrac.toDouble(x)

    override def compare(x: Complex[Double], y: Complex[Double]): Int =
      complexFrac.compare(x, y) // Using default comparison from Fractional

    // Provide a basic implementation for parseString
    override def parseString(str: String): Option[Complex[Double]] = {
      // A very simple parser implementation; you can replace this with a proper one
      try {
        val parts = str.split("\\+")
        if (parts.length == 2) {
          val real = parts(0).toDouble
          val imag = parts(1).stripSuffix("i").toDouble
          Some(Complex(real, imag))
        } else {
          Some(Complex(str.toDouble, 0))
        }
      } catch {
        case _: Exception => None
      }
    }
  }
}
