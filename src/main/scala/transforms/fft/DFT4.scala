package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.rtl.signals.Sig
import ir.spl.{Repeatable, SPL}
import linalg.Fields.Complex

/**
 * Class representing the Discrete Fourier Transform of size 4 (Radix-4).
 * 
 * @tparam T Numeric type of the inputs and outputs.
 * @param num Implicit Numeric instance for the type T.
 */
class DFT4[T](implicit val num: Numeric[T])
    extends SPL[T](2)    // Represents a Radix-4 DFT (2^2 = 4 points).
    with Repeatable[T] {

  /**
   * Evaluates the DFT4 operation on a sequence of inputs.
   *
   * @param inputs Sequence of inputs to process.
   * @param set Control parameter for set processing.
   * @return Sequence of transformed outputs.
   */
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs
    .grouped(4)  // Group inputs in sets of 4 for Radix-4 processing.
    .toSeq
    .flatMap(i => {
      // Sum and difference calculations for Radix-4 butterfly
      val sum1 = num.plus(i(0), i(1))
      val sum2 = num.plus(i(2), i(3))
      val diff1 = num.minus(i(0), i(1))
      val diff2 = num.minus(i(2), i(3))
      Seq(
        num.plus(sum1, sum2),   // Output 1: (x0 + x1) + (x2 + x3)
        num.minus(sum1, sum2),  // Output 2: (x0 + x1) - (x2 + x3)
        num.plus(diff1, diff2), // Output 3: (x0 - x1) + (x2 - x3)
        num.minus(diff1, diff2) // Output 4: (x0 - x1) - (x2 - x3)
      )
    })

  /**
   * Implements the streaming DFT4 module.
   * 
   * @param k Logarithm of the streaming width.
   * @param control RAMControl instance to manage memory access.
   * @param hw Implicit hardware type for T.
   * @return An AcyclicStreamingModule for the DFT4 operation.
   */
  override def stream(k: Int, control: RAMControl)(implicit
      hw: HW[T]
  ): AcyclicStreamingModule[T] = {
    require(k == 2, s"DFT4 requires k to be 2, but got k=$k")  // Ensure streaming width is correct
    println(s"DFT4 stream method called with k=$k, expected k=2")
    Butterfly4[T](k)  // Call the butterfly operation for Radix-4
  }
}

object DFT4 {
  /**
   * Factory method to create a new DFT4 instance.
   * 
   * @tparam T Numeric type for the inputs and outputs.
   * @param num Implicit Numeric instance for T.
   * @return A new DFT4 instance.
   */
  def apply[T: Numeric]() = new DFT4[T]()

  /**
   * Pattern matching extractor for DFT4.
   * 
   * @param arg SPL instance to check.
   * @tparam T Numeric type of the SPL instance.
   * @return True if the instance is of type DFT4, false otherwise.
   */
  def unapply[T](arg: SPL[T]): Boolean = arg match {
    case _: DFT4[T] => true
    case _          => false
  }
}

