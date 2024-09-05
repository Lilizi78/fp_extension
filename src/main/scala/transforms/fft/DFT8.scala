package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.rtl.signals.Sig
import ir.spl.{Repeatable, SPL}
import linalg.Fields.Complex

// Import ComplexOps to support complex operations and omega function for twiddle factors
import transforms.fft.ComplexOps
import transforms.fft.DFT.omega

/**
 * Class representing the Discrete Fourier Transform of size 8 (Radix-8).
 * This class supports the DFT operation using generic types that can represent
 * both real and complex numbers.
 *
 * @tparam T Numeric type of the inputs and outputs, supporting operations defined in ComplexOps.
 * @param ops Implicit ComplexOps instance for the type T.
 */
class DFT8[T](implicit val ops: ComplexOps[T])
    extends SPL[T](3)   // Represents a Radix-8 DFT (2^3 = 8 points).
    with Repeatable[T] {

  /**
   * Evaluates the DFT8 operation on a sequence of inputs.
   *
   * @param inputs Sequence of inputs to process.
   * @param set Control parameter for set processing (unused in this implementation).
   * @return Sequence of transformed outputs after applying the Radix-8 DFT.
   */
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs
    .grouped(8)  // Group inputs in sets of 8 for Radix-8 processing.
    .toSeq
    .flatMap(i => {
      // Compute Twiddle Factors for Radix-8. These factors are required for FFT computation.
      // They are computed using the omega function for different powers (0 to 7 for Radix-8).
      val W8_0 = ops.fromInt(1)                  // Twiddle factor W8^0 = 1
      val W8_1 = omega(3, 1).asInstanceOf[T]     // Twiddle factor W8^1
      val W8_2 = omega(3, 2).asInstanceOf[T]     // Twiddle factor W8^2
      val W8_3 = omega(3, 3).asInstanceOf[T]     // Twiddle factor W8^3
      val W8_4 = omega(3, 4).asInstanceOf[T]     // Twiddle factor W8^4
      val W8_5 = omega(3, 5).asInstanceOf[T]     // Twiddle factor W8^5
      val W8_6 = omega(3, 6).asInstanceOf[T]     // Twiddle factor W8^6
      val W8_7 = omega(3, 7).asInstanceOf[T]     // Twiddle factor W8^7

      // Sum and difference calculations for Radix-8 butterfly
      val sum1 = ops.plus(i(0), i(4))  // (x0 + x4)
      val sum2 = ops.plus(i(1), i(5))  // (x1 + x5)
      val sum3 = ops.plus(i(2), i(6))  // (x2 + x6)
      val sum4 = ops.plus(i(3), i(7))  // (x3 + x7)
      val diff1 = ops.minus(i(0), i(4)) // (x0 - x4)
      val diff2 = ops.minus(i(1), i(5)) // (x1 - x5)
      val diff3 = ops.minus(i(2), i(6)) // (x2 - x6)
      val diff4 = ops.minus(i(3), i(7)) // (x3 - x7)

      // Apply Twiddle Factors multiplication for all outputs
      Seq(
        ops.times(ops.plus(sum1, sum2), W8_0),                      // Output 1: ((x0 + x4) + (x1 + x5)) * W8^0
        ops.times(ops.plus(sum3, sum4), W8_1),                      // Output 2: ((x2 + x6) + (x3 + x7)) * W8^1
        ops.times(ops.minus(sum1, sum2), W8_2),                     // Output 3: ((x0 + x4) - (x1 + x5)) * W8^2
        ops.times(ops.minus(sum3, sum4), W8_3),                     // Output 4: ((x2 + x6) - (x3 + x7)) * W8^3
        ops.times(ops.plus(diff1, diff2), W8_4),                    // Output 5: ((x0 - x4) + (x1 - x5)) * W8^4
        ops.times(ops.plus(diff3, diff4), W8_5),                    // Output 6: ((x2 - x6) + (x3 - x7)) * W8^5
        ops.times(ops.minus(diff1, diff2), W8_6),                   // Output 7: ((x0 - x4) - (x1 - x5)) * W8^6
        ops.times(ops.minus(diff3, diff4), W8_7)                    // Output 8: ((x2 - x6) - (x3 - x7)) * W8^7
      )
    })

  /**
   * Implements the streaming DFT8 module using butterfly operations.
   * This method is critical for streaming FFT architectures, especially for FPGA and ASIC implementations.
   * 
   * @param k Logarithm of the streaming width.
   * @param control RAMControl instance to manage memory access.
   * @param hw Implicit hardware type for T.
   * @return An AcyclicStreamingModule for the DFT8 operation.
   */
  override def stream(k: Int, control: RAMControl)(implicit
      hw: HW[T]
  ): AcyclicStreamingModule[T] = {
    //require(k >= 3, s"DFT8 requires k to be at least 3, but got k=$k")  // Ensure streaming width is correct
    //println(s"DFT8 stream method called with k=$k")
    Butterfly8[T](k)  // Call the butterfly operation for Radix-8
  }
}

object DFT8 {
  /**
   * Factory method to create a new DFT8 instance.
   * 
   * @tparam T Numeric type for the inputs and outputs.
   * @param ops Implicit ComplexOps instance for T.
   * @return A new DFT8 instance.
   */
  def apply[T: ComplexOps]() = new DFT8[T]()

  /**
   * Pattern matching extractor for DFT8.
   * 
   * @param arg SPL instance to check.
   * @tparam T Numeric type of the SPL instance.
   * @return True if the instance is of type DFT8, false otherwise.
   */
  def unapply[T](arg: SPL[T]): Boolean = arg match {
    case _: DFT8[T] => true
    case _          => false
  }
}

