/*
package transforms.fft

import ir.rtl.hardwaretype.{HW, ComplexHW}
import ir.rtl.signals.Sig
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.spl.SPL
import transforms.fft.{Butterfly, DFT}
import linalg.Fields.Complex

/**
 * Radix22 module for FFT with support for Radix-2^2 decomposition.
 * This module is designed to work with the -d 2 parameter, indicating 
 * that the FFT should use a Radix-2^2 decomposition structure.
 * 
 * @param currentN Current size of the FFT transform, typically 2^n.
 * @param r Radix size, typically 2 for Radix-2.
 * @param currentK Streaming width parameter, determines the number of input/output ports.
 * @param decomp Boolean indicating whether Radix-2^2 decomposition is active.
 */
case class Radix22[T: HW](currentN: Int, r: Int, currentK: Int, decomp: Boolean) 
  extends AcyclicStreamingModule[T](currentN, currentK) {

  override def toString: String = s"Radix22 FFT, n=$currentN, r=$r, k=$currentK, decomp=$decomp"

  /**
   * Main implementation function based on Radix-2^2 structure.
   * This function handles the Radix-2^2 decomposition and processes 
   * each stage of the FFT recursively.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Implementing Radix-2^2 FFT: n=$currentN, r=$r, k=$currentK, decomp=$decomp")

    // Check if Radix-2^2 decomposition is enabled
    if (decomp) {
      // Group the inputs into sets of 4 to process with Radix-2^2
      val groupedInputs = inputs.grouped(4).toSeq
      println(s"Grouped inputs into ${groupedInputs.size} groups of 4")

      // Process each group using Radix-2^2 structure
      groupedInputs.zipWithIndex.flatMap { case (group, stage) =>
        println(s"Processing group: $group at stage $stage")
        processGroup(group, stage + 1)  // Stage numbers start from 1 for clarity
      }
    } else {
      // When decomp is false, fall back to the default Cooley-Tukey FFT
      val complexInputs = inputs.collect { case sig: Sig[Complex[Double]] => sig }
      if (complexInputs.size != inputs.size)
        throw new IllegalArgumentException("Expected all inputs to be of type Complex[Double]")
      
      // Evaluate FFT using DFT module
      println(s"Evaluating Cooley-Tukey FFT without decomposition")
      DFT.CTDFT(currentN, r).eval(complexInputs.map(c => extractComplexValue(c)), set = 0).map(complexToSig)
    }
  }

  /**
   * Extracts the complex value from a signal for FFT processing.
   */
  private def extractComplexValue(signal: Sig[Complex[Double]]): Complex[Double] = {
    signal match {
      case complexSig: Sig[Complex[Double]] => complexSig.hw.asInstanceOf[Complex[Double]]
      case _ => throw new IllegalArgumentException("Expected signal of type Complex[Double]")
    }
  }

  /**
   * Processes a group of 4 signals according to the current stage in Radix-2^2.
   */
  private def processGroup(group: Seq[Sig[T]], stage: Int): Seq[Sig[T]] = {
    println(s"Applying rotations for stage $stage")
    val rotatedGroup = applyRotations(group, stage)

    // Apply Butterfly operation to perform core Radix-2 computation
    println(s"Performing Butterfly operations on rotated group")
    val butterflyOutputs = Butterfly[T]().implement(rotatedGroup)
    println(s"Butterfly outputs: $butterflyOutputs")

    // Recursively apply further FFT processing if within expected stages
    if (stage < Math.log(currentN) / Math.log(r)) {
      println(s"Recursively applying Cooley-Tukey FFT for further processing")
      applyCTDFT(butterflyOutputs)
    } else {
      butterflyOutputs
    }
  }

  /**
   * Applies necessary rotations to signals based on stage index to align data for FFT processing.
   */
  private def applyRotations(group: Seq[Sig[T]], stage: Int): Seq[Sig[T]] = {
    group.zipWithIndex.map { case (signal, index) =>
      // Determine rotation based on the index and stage, handling trivial and non-trivial cases
      val rotationFactor = if (stage % 2 != 0) DFT.omega(2, index) else DFT.omega(currentN, index)
      val rotatedSignal = signal * complexToSig(rotationFactor)
      println(s"Rotated component at index $index: $rotatedSignal")
      rotatedSignal
    }
  }

  /**
   * Converts a Complex number to a hardware-compatible signal.
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
   * Applies the Cooley-Tukey FFT recursively to decomposed signal groups.
   */
  private def applyCTDFT(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    val complexInputs = inputs.collect { case sig: Sig[Complex[Double]] => sig }
    val fftStructure = DFT.CTDFT(complexInputs.size, r)
    println(s"Applying Cooley-Tukey FFT to further stages")
    fftStructure.eval(complexInputs.map(c => extractComplexValue(c)), set = 0).map(complexToSig)
  }

  /**
   * SPL representation of Radix-22 FFT structure.
   */
  override def spl: SPL[T] = {
    DFT.CTDFT(currentN, r).asInstanceOf[SPL[T]]
  }

  /**
   * Streaming implementation to handle data flow and manage hardware pipeline.
   */
  def stream(currentK: Int, control: RAMControl)(using hw: HW[T]): StreamingModule[T] = {
    println(s"Streaming implementation: k=$currentK")
    new StreamingModule[T](currentK, control.hashCode) {
      /**
       * Implement function defining the main signal processing logic for streaming.
       */
      override def implement(rst: ir.rtl.Component, token: Int => ir.rtl.Component, inputs: Seq[ir.rtl.Component]): Seq[ir.rtl.Component] = {
        println(s"Streaming module implement called with ${inputs.size} components")
        val groupedComponents = inputs.grouped(4).toSeq
        println(s"Grouped inputs into ${groupedComponents.size} groups of 4")

        groupedComponents.flatMap { group =>
          // Process each component group through Radix-2^2 structure
          group.map { component =>
            // Replace this placeholder with actual signal transformation logic
            println(s"Processing component in streaming: $component")
            token(0)
          }
        }
      }

      // Define latency based on the pipeline and stage requirements
      override def latency: Int = 1 // Adjust this based on the depth of the operations in your specific case

      // Override toString and spl for clear identification and processing
      override def toString: String = Radix22(currentN, r, currentK, decomp).toString
      override def spl: SPL[T] = Radix22(currentN, r, currentK, decomp).spl
    }
  }
}
*/