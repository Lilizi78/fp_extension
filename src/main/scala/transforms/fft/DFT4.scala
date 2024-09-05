package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.rtl.signals.Sig
import ir.spl.{Repeatable, SPL}
import linalg.Fields.Complex

// Import the ComplexOps type class to support complex operations
import transforms.fft.ComplexOps
import transforms.fft.DFT.omega

/**
 * Class representing the Discrete Fourier Transform of size 4 (Radix-4).
 * This class supports the DFT operation using generic types that can represent
 * both real and complex numbers.
 * 
 * @tparam T Numeric type of the inputs and outputs, supporting operations defined in ComplexOps.
 * @param ops Implicit ComplexOps instance for the type T.
 */
class DFT4[T](implicit val ops: ComplexOps[T])
    extends SPL[T](2)     // Represents a Radix-4 DFT (2^2 = 4 points).
    with Repeatable[T] {

  /**
   * Evaluates the DFT4 operation on a sequence of inputs.
   *
   * @param inputs Sequence of inputs to process.
   * @param set Control parameter for set processing (unused in this implementation).
   * @return Sequence of transformed outputs after applying the Radix-4 DFT.
   */
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs
    .grouped(4)  // Group inputs in sets of 4 for Radix-4 processing.
    .toSeq
    .flatMap(i => {
      // Compute Twiddle Factors for Radix-4. These factors are required for FFT computation.
      // They are computed using the omega function for different powers (0 to 3 for Radix-4).
      val W4_0 = ops.fromInt(1)                  // Twiddle factor W4^0 = 1
      val W4_1 = omega(2, 1).asInstanceOf[T]  // Twiddle factor W4^1
      val W4_2 = omega(2, 2).asInstanceOf[T]  // Twiddle factor W4^2
      val W4_3 = omega(2, 3).asInstanceOf[T]  // Twiddle factor W4^3

      // Sum and difference calculations for Radix-4 butterfly
      val sum1 = ops.plus(i(0), i(1))  // (x0 + x1)
      val sum2 = ops.plus(i(2), i(3))  // (x2 + x3)
      val diff1 = ops.minus(i(0), i(1)) // (x0 - x1)
      val diff2 = ops.minus(i(2), i(3)) // (x2 - x3)

      // Apply Twiddle Factors multiplication for all outputs
      Seq(
        ops.times(ops.plus(sum1, sum2), W4_0),   // Output 1: ((x0 + x1) + (x2 + x3)) * W4^0
        ops.times(ops.minus(sum1, sum2), W4_1),   // Output 2: ((x0 + x1) - (x2 + x3)) * W4^1
        ops.times(ops.plus(diff1, diff2), W4_2),  // Output 3: ((x0 - x1) + (x2 - x3)) * W4^2
        ops.times(ops.minus(diff1, diff2), W4_3)  // Output 4: ((x0 - x1) - (x2 - x3)) * W4^3
      )
    })

  /**
   * Implements the streaming DFT4 module using butterfly operations.
   * This method is critical for streaming FFT architectures, especially for FPGA and ASIC implementations.
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
   * @param ops Implicit ComplexOps instance for T.
   * @return A new DFT4 instance.
   */
  def apply[T: ComplexOps]() = new DFT4[T]()

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
