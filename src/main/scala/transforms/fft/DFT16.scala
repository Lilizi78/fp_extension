package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.rtl.signals.Sig
import ir.spl.{Repeatable, SPL}
import linalg.Fields.Complex

/**
 * DFT16 class for performing radix-16 Discrete Fourier Transform (DFT).
 *
 * @param num Numeric evidence for type T
 * @tparam T Type parameter representing the numeric type
 */
class DFT16[T](implicit val num: Numeric[T])
    extends SPL[T](4)
    with Repeatable[T] {
  
  /**
   * Evaluates the DFT16 transform on the provided inputs.
   *
   * @param inputs The sequence of inputs
   * @param set Ignored for now (could be used for controlling specific settings)
   * @return The transformed sequence of inputs
   */
  override def eval(inputs: Seq[T], set: Int): Seq[T] = {
    // Group the inputs in batches of 16 and apply the radix-16 DFT
    inputs.grouped(16).toSeq.flatMap { i =>
      val sum1 = num.plus(i(0), i(8))
      val sum2 = num.plus(i(1), i(9))
      val sum3 = num.plus(i(2), i(10))
      val sum4 = num.plus(i(3), i(11))
      val sum5 = num.plus(i(4), i(12))
      val sum6 = num.plus(i(5), i(13))
      val sum7 = num.plus(i(6), i(14))
      val sum8 = num.plus(i(7), i(15))

      val diff1 = num.minus(i(0), i(8))
      val diff2 = num.minus(i(1), i(9))
      val diff3 = num.minus(i(2), i(10))
      val diff4 = num.minus(i(3), i(11))
      val diff5 = num.minus(i(4), i(12))
      val diff6 = num.minus(i(5), i(13))
      val diff7 = num.minus(i(6), i(14))
      val diff8 = num.minus(i(7), i(15))

      Seq(
        num.plus(sum1, sum2), num.plus(sum3, sum4), num.plus(sum5, sum6), num.plus(sum7, sum8),
        num.minus(sum1, sum2), num.minus(sum3, sum4), num.minus(sum5, sum6), num.minus(sum7, sum8),
        num.plus(diff1, diff2), num.plus(diff3, diff4), num.plus(diff5, diff6), num.plus(diff7, diff8),
        num.minus(diff1, diff2), num.minus(diff3, diff4), num.minus(diff5, diff6), num.minus(diff7, diff8)
      )
    }
  }

  /**
   * Returns a streaming module for the DFT16 transform.
   *
   * @param k Streaming width
   * @param control RAM control for handling input/output flow
   * @param hw Hardware type evidence for T
   * @return An acyclic streaming module for DFT16
   */
  override def stream(k: Int, control: RAMControl)(implicit hw: HW[T]): AcyclicStreamingModule[T] = {
    Butterfly16[T](k)  // Use Butterfly16 to perform the streaming transform
  }
}

object DFT16 {
  /**
   * Factory method to create a DFT16 instance.
   *
   * @tparam T Type parameter representing the numeric type
   * @param num Numeric evidence for type T
   * @return A new DFT16 instance
   */
  def apply[T: Numeric](): DFT16[T] = new DFT16[T]()

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
