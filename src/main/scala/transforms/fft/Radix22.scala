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
 * @param logN      The logarithm of the total number of input points for the FFT (currentN = 2^logN).
 * @param logK      The logarithm of the streaming width of the FFT (currentK = 2^logK).
 * @param r         The log2 of the radix used (currentR = 2^r).
 * @param decomp    Boolean flag to indicate if decomposition is used in the FFT.
 */
case class Radix22[T: HW](logN: Int, logK: Int, r: Int, decomp: Boolean)
  extends AcyclicStreamingModule[T](Math.pow(2, logN).toInt, Math.pow(2, logK).toInt) {

  // Correctly calculate currentN, currentK, and currentR based on the input parameters.
  private val currentN = Math.pow(2, logN).toInt
  private val currentK = Math.pow(2, logK).toInt
  private val currentR = Math.pow(2, r).toInt

  // Calculate total stages based on the logarithm of currentN.
  private val totalStages = logN

  override def toString: String = s"Radix22 FFT, n=$logN, r=$r, k=$logK, decomp=$decomp"

  // Function to compute the expected input size at each stage
  private def expectedSize(stage: Int): Int = {
    (currentN / Math.pow(4, stage - 1)).toInt
  }

  /**
   * Main implementation function for Radix-2^2 FFT.
   * Processes the input signal by either decomposing into smaller sub-FFT units or applying
   * a single Cooley-Tukey FFT computation depending on the `decomp` flag.
   *
   * @param inputs  Sequence of input signals to be processed.
   * @return        Sequence of processed signals after FFT computation.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Implementing Radix-2^2 FFT: n=$logN, r=$r, k=$logK, decomp=$decomp")

    if (decomp) {
      val groupSize = determineGroupSize(stage = 1)
      val groupedInputs = inputs.grouped(groupSize).toSeq
      println(s"Grouped inputs into ${groupedInputs.size} groups of $groupSize")

      groupedInputs.zipWithIndex.flatMap { case (group, stage) =>
        println(s"Processing group: $group at stage ${stage + 1}")
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
    // Radix-2^2 FFT has a group size of 2 for each stage due to its structure.
    2
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

    // Step 1: Apply trivial rotations
    val rotatedGroup = applyRotations(group, stage)
    println(s"Stage $stage: Rotated group size: ${rotatedGroup.size}")

    // Step 2: Perform Butterfly operations
    val butterflyOutputs = Butterfly[T]().implement(rotatedGroup)
    println(s"Stage $stage: Butterfly outputs size: ${butterflyOutputs.size}")

    // Step 3: Check Butterfly outputs to ensure they are correct
    if (butterflyOutputs.size != group.size) {
      throw new IllegalArgumentException(s"Incorrect Butterfly output size at stage $stage. Expected ${group.size}, got ${butterflyOutputs.size}.")
    }

    // Step 4: Apply recursive FFT for further processing, pass the outputs to the next stage or return them
    if (stage < totalStages) {
      // Combine all butterfly outputs into the next stage expected size
      val combinedOutputs = butterflyOutputs.grouped(expectedSize(stage + 1)).toSeq.flatten
      println(s"Stage $stage: Combining outputs into expected size ${expectedSize(stage + 1)} for the next stage")

      // Check if combined size matches what is required for the next stage
      if (combinedOutputs.size != expectedSize(stage + 1)) {
        throw new IllegalArgumentException(s"Combined output size mismatch at stage $stage. Expected ${expectedSize(stage + 1)}, got ${combinedOutputs.size}.")
      }

      println(s"Stage $stage: Applying recursive FFT for further processing")
      applyCTDFT(combinedOutputs, stage + 1)
    } else {
      butterflyOutputs
    }
  }

  /**
   * Applies trivial rotations to the group of input signals based on the current stage and index.
   *
   * @param group  The group of input signals to rotate.
   * @param stage  The current stage number of the FFT computation.
   * @return       A sequence of rotated signals with only trivial factors applied.
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
   * Adjusts for Decimation in Time (DIT) requirements, handling trivial and non-trivial rotations.
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
      // Handle trivial rotations at odd stages
      trivialRotations(index % 4)
    } else {
      // Non-trivial rotations are handled in CTBF at the even stages
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

    // Update expectedSize to align with Radix-2^2 FFT requirements
    val expectedSize = (currentN / Math.pow(4, stage - 1)).toInt
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

        // Dynamically determine group size based on stage
        val groupSize = determineGroupSize(1)
        val groupedComponents = inputs.grouped(groupSize).toSeq
        println(s"Grouped inputs into ${groupedComponents.size} groups of $groupSize")

        // Adjust processing based on stage and total stages logic
        groupedComponents.flatMap { group =>
          processGroup(group, 1, totalStages = logN)
        }
      }

      override def toString: String = Radix22(logN, logK, r, decomp).toString
      override def spl: SPL[T] = Radix22(logN, logK, r, decomp).spl
    }
  }
}
