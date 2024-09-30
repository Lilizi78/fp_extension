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

  private val currentN = Math.pow(2, logN).toInt
  private val currentK = Math.pow(2, logK).toInt
  private val currentR = Math.pow(2, r).toInt
  private val totalStages = logN

  override def toString: String = s"Radix22 FFT, n=$logN, r=$r, k=$logK, decomp=$decomp"

  /**
   * Main implementation function for Radix-2^2 FFT.
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
   */
  private def determineGroupSize(stage: Int): Int = currentN / Math.pow(2, stage).toInt

  private def fixedPointToDouble(fp: FixedPoint): Double = fp.valueOf(fp.bitsOf(1))

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
   */
  private def processGroup(group: Seq[Sig[T]], stage: Int, totalStages: Int): Seq[Sig[T]] = {
    println(s"Stage $stage of $totalStages: Processing group with ${group.size} components")

    // Step 1: Apply rotations
    val rotatedGroup = applyRotations(group, stage)
    println(s"Stage $stage: Rotated group size: ${rotatedGroup.size}")

    // Step 2: Perform Butterfly operations
    val butterflyOutputs = Butterfly[T]().implement(rotatedGroup)
    println(s"Stage $stage: Butterfly outputs size: ${butterflyOutputs.size}")

    // Step 3: Ensure butterfly outputs are correct
    if (butterflyOutputs.size != group.size) {
      throw new IllegalArgumentException(s"Incorrect Butterfly output size at stage $stage. Expected ${group.size}, got ${butterflyOutputs.size}.")
    }

    // Step 4: Buffer and shuffle outputs for the next stage
    val bufferedOutputs = bufferAndShuffle(butterflyOutputs, stage)
    println(s"Stage $stage: Buffered outputs processed for next stage")

    // Step 5: Dynamically calculate next stage size based on Radix-2^2 structure
    val nextStageSize = (currentN / (4 * stage)).toInt
    if (nextStageSize < 1) {
      throw new IllegalArgumentException(s"Invalid next stage size at stage $stage.")
    }

    // Step 6: Apply recursive FFT if necessary
    if (stage < totalStages) {
      val combinedOutputs = bufferedOutputs.grouped(nextStageSize).toSeq.flatten
      println(s"Stage $stage: Combining outputs into expected size $nextStageSize for the next stage")

      if (combinedOutputs.size != nextStageSize) {
        throw new IllegalArgumentException(s"Combined output size mismatch at stage $stage. Expected $nextStageSize, got ${combinedOutputs.size}.")
      }

      println(s"Stage $stage: Applying recursive FFT for further processing")
      applyCTDFT(combinedOutputs, stage + 1)
    } else {
      bufferedOutputs
    }
  }

  /**
  * Buffers and shuffles outputs based on the current stage to ensure correct data ordering.
  */
  private def bufferAndShuffle(outputs: Seq[Sig[T]], stage: Int): Seq[Sig[T]] = {
    // Dynamically calculate the group size, which decreases as the stage increases.
    val groupSize = determineGroupSize(stage)

    // Split the current output data into two parts: top and bottom.
    val (top, bottom) = outputs.splitAt(groupSize)

    // Reorder the data based on whether the stage is even or odd.
    if (stage % 2 == 0) {
      // For even stages: concatenate top part first, then the bottom part.
      top ++ bottom
    } else {
      // For odd stages: concatenate bottom part first, then the top part.
      bottom ++ top
    }
  }

  /**
   * Applies the necessary rotations to the group of input signals based on the current stage.
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
   * Computes the appropriate twiddle factor based on the stage and index.
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

  private def applyCTDFT(inputs: Seq[Sig[T]], stage: Int): Seq[Sig[T]] = {
    val complexInputs = inputs.collect { case sig: Sig[Complex[Double]] => sig }
    if (complexInputs.size != inputs.size) {
      throw new IllegalArgumentException("Mismatch in complex input size during FFT processing.")
    }

    val nextStageSize = determineGroupSize(stage + 1)
    if (complexInputs.size != nextStageSize) {
      throw new IllegalArgumentException(s"Invalid input size at stage $stage. Expected $nextStageSize, but got ${complexInputs.size}.")
    }

    val fftStructure = DFT.CTDFT(complexInputs.size, r)
    println(s"Applying Cooley-Tukey FFT to further stages at Stage $stage")

    val result = fftStructure.eval(complexInputs.map(c => extractComplexValue(c)), set = 0)
    if (result.size != complexInputs.size) {
      throw new IllegalArgumentException(s"FFT result size mismatch: expected ${complexInputs.size}, got ${result.size}.")
    }

    result.map(complexToSig)
  }

  override def spl: SPL[T] = DFT.CTDFT(currentN, r).asInstanceOf[SPL[T]]

  def stream(currentK: Int, control: RAMControl)(using hw: HW[T]): AcyclicStreamingModule[T] = {
    println(s"Streaming implementation: k=$currentK")
    new AcyclicStreamingModule[T](currentN, currentK) {
      override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
        println(s"Streaming module implement called with ${inputs.size} components")

        // Dynamically adjust group size based on current stage
        val stage = 1
        val groupSize = determineGroupSize(stage)
        val groupedComponents = inputs.grouped(groupSize).toSeq

        println(s"Grouped inputs into ${groupedComponents.size} groups of $groupSize")

        // Process each group and pass it to the next stages recursively
        val processedGroups = groupedComponents.flatMap { group =>
          processGroup(group, stage, totalStages)
        }

        // Return the processed groups as the final result
        processedGroups
      }

      override def toString: String = Radix22(logN, logK, r, decomp).toString
      override def spl: SPL[T] = Radix22(logN, logK, r, decomp).spl
    }
  }
}
