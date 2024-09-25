package transforms.fft

import ir.rtl.hardwaretype.{HW, FixedPoint, ComplexHW}
import ir.rtl.signals.Sig
import ir.rtl.{AcyclicStreamingModule, RAMControl}
import ir.spl.SPL
import transforms.fft.{Butterfly, DFT}
import linalg.Fields.Complex

/**
 * Radix22 FFT implementation using Radix-2^2 decomposition.
 *
 * @param currentN   The total number of input points for the FFT.
 * @param r          The log2 of the radix used (e.g., for Radix-2, r=1; for Radix-4, r=2).
 * @param currentK   The streaming width of the FFT (number of parallel input streams).
 * @param decomp     Boolean flag to indicate if decomposition is used in the FFT.
 */
case class Radix22[T: HW](currentN: Int, r: Int, currentK: Int, decomp: Boolean)
  extends AcyclicStreamingModule[T](currentN, currentK) {

  override def toString: String = s"Radix22 FFT, n=$currentN, r=$r, k=$currentK, decomp=$decomp"

  /**
   * Main implementation function for Radix-2^2 FFT.
   * Processes the input signal by either decomposing into smaller sub-FFT units or applying
   * a single Cooley-Tukey FFT computation depending on the `decomp` flag.
   *
   * @param inputs  Sequence of input signals to be processed.
   * @return        Sequence of processed signals after FFT computation.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Implementing Radix-2^2 FFT: n=$currentN, r=$r, k=$currentK, decomp=$decomp")

    // Calculate total stages based on log2(N)
    val totalStages = (Math.log(currentN) / Math.log(2)).toInt

    if (decomp) {
      val groupSize = determineGroupSize(stage = 1)
      val groupedInputs = inputs.grouped(groupSize).toSeq
      println(s"Grouped inputs into ${groupedInputs.size} groups of $groupSize")

      groupedInputs.zipWithIndex.flatMap { case (group, stage) =>
        println(s"Processing group: $group at stage ${stage + 1}")
        // Pass totalStages to processGroup
        processGroup(group, stage + 1, totalStages)
      }
    } else {
      val complexInputs = inputs.collect { case sig: Sig[Complex[Double]] => sig }
      if (complexInputs.size != inputs.size)
        throw new IllegalArgumentException("Expected all inputs to be of type Complex[Double]")

      println(s"Evaluating Cooley-Tukey FFT without decomposition")
      DFT.CTDFT(currentN, r).eval(complexInputs.map(c => extractComplexValue(c)), set = 0).map(complexToSig)
    }
  }

  /**
   * Determines the correct grouping size for the inputs based on the current stage and radix settings.
   *
   * @param stage The current processing stage.
   * @return      The size of groups for the current stage.
   */
  private def determineGroupSize(stage: Int): Int = {
    // Radix-2^2 FFT typically doubles group size each stage
    Math.pow(2, stage).toInt
  }

  /**
   * Converts FixedPoint values to Double using the valueOf method from FixedPoint.
   *
   * @param fp  The FixedPoint value to be converted.
   * @return    The corresponding Double value.
   */
  private def fixedPointToDouble(fp: FixedPoint): Double = {
    fp.valueOf(fp.bitsOf(1))
  }

  /**
   * Extracts complex values from a signal for FFT processing, converting them to
   * Complex[Double] types.
   *
   * @param signal  The input signal containing complex values.
   * @return        A Complex[Double] value extracted from the signal.
   */
  private def extractComplexValue(signal: Sig[Complex[Double]]): Complex[Double] = {
    signal.hw match {
      case complexHW: ComplexHW[T] =>
        val realValue = fixedPointToDouble(signal.re.asInstanceOf[Sig[FixedPoint]].hw.asInstanceOf[FixedPoint])
        val imagValue = fixedPointToDouble(signal.im.asInstanceOf[Sig[FixedPoint]].hw.asInstanceOf[FixedPoint])
        Complex(realValue, imagValue)
      case _ =>
        throw new IllegalArgumentException("Expected signal of type ComplexHW.")
    }
  }

  /**
   * Processes a group of signals at a given stage, applying rotations, butterfly computations,
   * and recursively calling the next stages of FFT as necessary.
   *
   * @param group       The group of input signals to process.
   * @param stage       The current stage number of the FFT computation.
   * @param totalStages The total number of stages in the FFT process.
   * @return            A sequence of processed signals.
   */
  private def processGroup(group: Seq[Sig[T]], stage: Int, totalStages: Int): Seq[Sig[T]] = {
    println(s"Stage $stage of $totalStages: Processing group with ${group.size} components")
    val rotatedGroup = applyRotations(group, stage)
    println(s"Stage $stage: Rotated group size: ${rotatedGroup.size}")

    val butterflyOutputs = Butterfly[T]().implement(rotatedGroup)
    println(s"Stage $stage: Butterfly outputs size: ${butterflyOutputs.size}")

    if (stage < totalStages) {
      println(s"Stage $stage: Applying recursive FFT for further processing")
      applyCTDFT(butterflyOutputs, stage + 1)
    } else {
      butterflyOutputs
    }
  }

  /**
   * Applies necessary rotations to the group of input signals based on the current stage and index,
   * computing appropriate twiddle factors.
   *
   * @param group  The group of input signals to rotate.
   * @param stage  The current stage number of the FFT computation.
   * @return       A sequence of rotated signals.
   */
  private def applyRotations(group: Seq[Sig[T]], stage: Int): Seq[Sig[T]] = {
    group.zipWithIndex.map { case (signal, index) =>
      val rotationFactor = computeTwiddleFactor(stage, index, currentN)
      val rotatedSignal = signal * complexToSig(rotationFactor)
      println(s"Rotated component at index $index: $rotatedSignal")
      rotatedSignal
    }
  }

  /**
   * Computes the correct Twiddle Factor for the given stage and index.
   * Adjusts for Decimation in Time (DIT) requirements, handling odd and even stages differently.
   *
   * @param stage  The current stage number.
   * @param index  The index within the stage.
   * @param n      The total number of input points.
   * @return       The computed Complex[Double] Twiddle Factor.
   */
  private def computeTwiddleFactor(stage: Int, index: Int, n: Int): Complex[Double] = {
    val trivialRotations = Seq(
      Complex(1.0, 0.0),   // 0°: no rotation
      Complex(0.0, -1.0),  // 90°: multiply by -j
      Complex(-1.0, 0.0),  // 180°: multiply by -1
      Complex(0.0, 1.0)    // 270°: multiply by j
    )

    if (stage % 2 != 0) {
      trivialRotations(index % 4)
    } else {
      DFT.omega(n, index)
    }
  }

  /**
   * Converts a Complex[Double] value to a signal (Sig[T]), used for interfacing with hardware-specific types.
   *
   * @param c   The complex number to be converted.
   * @param hw  Implicit hardware type information.
   * @return    A signal representation of the complex number.
   */
  private def complexToSig(c: Complex[Double])(using hw: HW[T]): Sig[T] = {
    hw match {
      case complexHW: ComplexHW[T] =>
        val realPart = complexHW.innerHW.valueOf(BigInt(c.re.toLong))
        val imagPart = complexHW.innerHW.valueOf(BigInt(c.im.toLong))
        complexHW.createComplexSignal(realPart, imagPart)
      case _ =>
        throw new IllegalArgumentException("HW[T] does not support Complex conversion directly.")
    }
  }

  /**
   * Applies Cooley-Tukey Decimation in Time FFT recursively, processing further stages
   * based on the current input signals and stage number.
   *
   * @param inputs  The current input signals to be processed.
   * @param stage   The current stage number.
   * @return        The processed signals after the FFT application.
   */
  private def applyCTDFT(inputs: Seq[Sig[T]], stage: Int): Seq[Sig[T]] = {
    val complexInputs = inputs.collect { case sig: Sig[Complex[Double]] => sig }
    if (complexInputs.size != inputs.size) {
      throw new IllegalArgumentException("Mismatch in complex input size during FFT processing.")
    }

    val expectedSize = currentN / Math.pow(4, stage - 1).toInt
    if (complexInputs.size != expectedSize) {
      throw new IllegalArgumentException(s"Invalid input size at stage $stage. Expected $expectedSize, but got ${complexInputs.size}.")
    }

    val fftStructure = DFT.CTDFT(complexInputs.size, r)
    println(s"Applying Cooley-Tukey FFT to further stages at Stage $stage")

    val result = fftStructure.eval(complexInputs.map(c => extractComplexValue(c)), set = 0)
    if (result.size != complexInputs.size) {
      throw new IllegalArgumentException(s"FFT result size mismatch: expected ${complexInputs.size}, got ${result.size}.")
    }

    result.map(complexToSig)
  }

  /**
   * Returns the SPL representation of the current Radix-2^2 FFT configuration, useful for
   * describing the overall structure of the FFT.
   */
  override def spl: SPL[T] = {
    DFT.CTDFT(currentN, r).asInstanceOf[SPL[T]]
  }

  /**
   * Creates a streaming version of the Radix-2^2 FFT, which can be used for hardware implementations.
   *
   * @param currentK  The current streaming width.
   * @param control   Control signals for RAM access.
   * @param hw        Implicit hardware type information.
   * @return          A new AcyclicStreamingModule for streaming FFT operations.
   */
  def stream(currentK: Int, control: RAMControl)(using hw: HW[T]): AcyclicStreamingModule[T] = {
    println(s"Streaming implementation: k=$currentK")
    new AcyclicStreamingModule[T](currentN, currentK) {
      override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
        println(s"Streaming module implement called with ${inputs.size} components")
        val groupedComponents = inputs.grouped(determineGroupSize(1)).toSeq
        println(s"Grouped inputs into ${groupedComponents.size} groups of ${determineGroupSize(1)}")

        groupedComponents.flatMap { group =>
          processGroup(group, 1, totalStages = (Math.log(currentN) / Math.log(2)).toInt)
        }
      }

      override def toString: String = Radix22(currentN, r, currentK, decomp).toString
      override def spl: SPL[T] = Radix22(currentN, r, currentK, decomp).spl
    }
  }
}
