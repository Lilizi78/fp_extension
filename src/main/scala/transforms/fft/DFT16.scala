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
 * DFT16 class for performing radix-16 Discrete Fourier Transform (DFT).
 * This class implements a radix-16 FFT using butterfly operations to transform 
 * a sequence of 16 complex points.
 *
 * @tparam T Type parameter representing the numeric type (typically Complex[Double])
 * @param ops Implicit evidence for ComplexOps type class
 */
class DFT16[T](implicit val ops: ComplexOps[T]) 
  extends SPL[T](4)  // Represents a Radix-16 DFT (2^4 = 16 points).
  with Repeatable[T] {

  /**
   * Evaluates the DFT16 transform on the provided inputs.
   * This method processes input sequences in groups of 16 and computes the 
   * radix-16 DFT using butterfly operations and twiddle factors.
   *
   * @param inputs The sequence of inputs to transform
   * @param set Ignored for now (could be used for controlling specific settings)
   * @return The transformed sequence of outputs
   */
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs
    .grouped(16)  // Process inputs in groups of 16 for radix-16 DFT
    .toSeq
    .flatMap( i =>{ 
      // Compute Twiddle Factors for Radix-16 using the omega function for various powers
      val W16_1 = omega(4, 1).asInstanceOf[T]  // W16^1
      val W16_2 = omega(4, 2).asInstanceOf[T]  // W16^2
      val W16_3 = omega(4, 3).asInstanceOf[T]  // W16^3
      val W16_4 = omega(4, 4).asInstanceOf[T]  // W16^4
      val W16_5 = omega(4, 5).asInstanceOf[T]  // W16^5
      val W16_6 = omega(4, 6).asInstanceOf[T]  // W16^6
      val W16_7 = omega(4, 7).asInstanceOf[T]  // W16^7
      val W16_8 = omega(4, 8).asInstanceOf[T]  // W16^8
      val W16_9 = omega(4, 9).asInstanceOf[T]  // W16^9
      val W16_10 = omega(4, 10).asInstanceOf[T] // W16^10
      val W16_11 = omega(4, 11).asInstanceOf[T] // W16^11
      val W16_12 = omega(4, 12).asInstanceOf[T] // W16^12
      val W16_13 = omega(4, 13).asInstanceOf[T] // W16^13
      val W16_14 = omega(4, 14).asInstanceOf[T] // W16^14
      val W16_15 = omega(4, 15).asInstanceOf[T] // W16^15

      // Sum and difference calculations for Radix-16 butterfly
      val sum1 = ops.plus(i(0), i(8))  // (x0 + x8)
      val sum2 = ops.plus(i(1), i(9))  // (x1 + x9)
      val sum3 = ops.plus(i(2), i(10)) // (x2 + x10)
      val sum4 = ops.plus(i(3), i(11)) // (x3 + x11)
      val sum5 = ops.plus(i(4), i(12)) // (x4 + x12)
      val sum6 = ops.plus(i(5), i(13)) // (x5 + x13)
      val sum7 = ops.plus(i(6), i(14)) // (x6 + x14)
      val sum8 = ops.plus(i(7), i(15)) // (x7 + x15)

      val diff1 = ops.minus(i(0), i(8)) // (x0 - x8)
      val diff2 = ops.minus(i(1), i(9)) // (x1 - x9)
      val diff3 = ops.minus(i(2), i(10))// (x2 - x10)
      val diff4 = ops.minus(i(3), i(11))// (x3 - x11)
      val diff5 = ops.minus(i(4), i(12))// (x4 - x12)
      val diff6 = ops.minus(i(5), i(13))// (x5 - x13)
      val diff7 = ops.minus(i(6), i(14))// (x6 - x14)
      val diff8 = ops.minus(i(7), i(15))// (x7 - x15)

      // Compute the outputs by applying the sums, differences, and twiddle factors
      Seq(
        ops.plus(sum1, sum2),                        // Output 1: (x0 + x8) + (x1 + x9)
        ops.times(ops.minus(sum1, sum2), W16_1),     // Output 2: ((x0 + x8) - (x1 + x9)) * W16^1
        ops.times(ops.plus(sum3, sum4), W16_2),      // Output 3: ((x2 + x10) + (x3 + x11)) * W16^2
        ops.times(ops.minus(sum3, sum4), W16_3),     // Output 4: ((x2 + x10) - (x3 + x11)) * W16^3
        ops.times(ops.plus(sum5, sum6), W16_4),      // Output 5: ((x4 + x12) + (x5 + x13)) * W16^4
        ops.times(ops.minus(sum5, sum6), W16_5),     // Output 6: ((x4 + x12) - (x5 + x13)) * W16^5
        ops.times(ops.plus(sum7, sum8), W16_6),      // Output 7: ((x6 + x14) + (x7 + x15)) * W16^6
        ops.times(ops.minus(sum7, sum8), W16_7),     // Output 8: ((x6 + x14) - (x7 + x15)) * W16^7
        ops.times(ops.plus(diff1, diff2), W16_8),      // Output 9: ((x0 - x8) + (x1 - x9)) * W16^8
        ops.times(ops.minus(diff1, diff2), W16_9),     // Output 10: ((x0 - x8) - (x1 - x9)) * W16^9
        ops.times(ops.plus(diff3, diff4), W16_10),     // Output 11: ((x2 - x10) + (x3 - x11)) * W16^10
        ops.times(ops.minus(diff3, diff4), W16_11),    // Output 12: ((x2 - x10) - (x3 - x11)) * W16^11
        ops.times(ops.plus(diff5, diff6), W16_12),     // Output 13: ((x4 - x12) + (x5 - x13)) * W16^12
        ops.times(ops.minus(diff5, diff6), W16_13),    // Output 14: ((x4 - x12) - (x5 - x13)) * W16^13
        ops.times(ops.plus(diff7, diff8), W16_14),     // Output 15: ((x6 - x14) + (x7 - x15)) * W16^14
        ops.times(ops.minus(diff7, diff8), W16_15)     // Output 16: ((x6 - x14) - (x7 - x15)) * W16^15
      )
    })
  

  /**
   * Returns a streaming module for the DFT16 transform.
   * The streaming module handles input/output flow efficiently using butterfly operations.
   *
   * @param k Streaming width
   * @param control RAM control for managing memory access
   * @param hw Hardware type evidence for T
   * @return An acyclic streaming module for DFT16
   */
  override def stream(k: Int, control: RAMControl)(implicit 
      hw: HW[T]
  ): AcyclicStreamingModule[T] = {
    Butterfly16[T](k)  // Use Butterfly16 to perform the streaming transform
  }
}

object DFT16 {
  /**
   * Factory method to create a DFT16 instance.
   *
   * @tparam T Type parameter representing the numeric type (e.g., Complex[Double])
   * @param ops Implicit evidence for ComplexOps type class
   * @return A new DFT16 instance
   */
  def apply[T: ComplexOps]() = new DFT16[T]()

  /**
   * Extractor method to check if an SPL is a DFT16.
   *
   * @param arg The SPL instance
   * @tparam T Type parameter representing the numeric type
   * @return Boolean indicating if the SPL is a DFT16
   */
  def unapply[T](arg: SPL[T]): Boolean = arg match {
    case _: DFT16[T] => true
    case _           => false
  }
}
