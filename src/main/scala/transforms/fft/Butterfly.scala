/*
 *    _____ ______          SGen - A Generator of Streaming Hardware
 *   / ___// ____/__  ____  Department of Computer Science, ETH Zurich, Switzerland
 *   \__ \/ / __/ _ \/ __ \
 *  ___/ / /_/ /  __/ / / / Copyright (C) 2020-2021 François Serre (serref@inf.ethz.ch)
 * /____/\____/\___/_/ /_/  https://github.com/fserre/sgen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *   
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *   
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 *   
 */

package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.signals.Sig
import ir.rtl.{AcyclicStreamingModule, StreamingModule}
import ir.spl.SPL
import transforms.fft.{DFT2, DFT4, DFT8, DFT16}

/**
 * Butterfly class for Radix-2 FFT computation.
 * This class extends an acyclic streaming module for computing the FFT using Radix-2.
 *
 * @tparam T The data type for the input signals, parameterized with hardware type.
 */
case class Butterfly[T: HW]() extends AcyclicStreamingModule[T](0, 1) {
  override def toString: String = "F2"

  /**
   * Implements the butterfly operation for Radix-2.
   *
   * @param inputs A sequence of input signals.
   * @return A sequence of output signals after performing the Radix-2 butterfly computation.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Butterfly: Implementing with ${inputs.size} inputs.")
    // Group inputs by 2 and perform addition and subtraction for each pair.
    inputs.grouped(2).toSeq.flatMap(i => Seq(i.head + i.last, i.head - i.last))
  }

  /**
   * Returns the SPL representation of this module, which in this case is a DFT of size 2.
   */
  override def spl: SPL[T] = DFT2[T]()(implicitly[HW[T]].num)
}

/**
 * Butterfly class for Radix-4 FFT computation.
 *
 * @param k Logarithm of the input size (i.e., the streaming width).
 * @tparam T The data type for the input signals, parameterized with hardware type.
 */
case class Butterfly4[T: HW : ComplexOps](override val k: Int) extends AcyclicStreamingModule[T](0, k) {
  override def toString: String = "F4"

  /**
   * Implements the butterfly operation for Radix-4.
   *
   * @param inputs A sequence of input signals.
   * @return A sequence of output signals after performing the Radix-4 butterfly computation.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Butterfly4: Implementing with ${inputs.size} inputs.")
    
    val inputSize = 1 << 2  // Calculate expected input size as 2^2

    // Ensure the number of inputs is a multiple of 2^k; if not, pad with zeros.
    val paddedInputs = if (inputs.size % inputSize != 0) {
      println(s"Warning: Input size is not a multiple of $inputSize, padding with zeros. Current size: ${inputs.size}")
      inputs ++ Seq.fill(inputSize - (inputs.size % inputSize))(inputs.head - inputs.head)  // Padding with zeros
    } else {
      inputs
    }

    // Process input groups based on calculated size
    paddedInputs.grouped(inputSize).toSeq.flatMap { i =>
      // Radix-4 butterfly computation
      val sum1 = i(0) + i(1)
      val sum2 = i(2) + i(3)
      val diff1 = i(0) - i(1)
      val diff2 = i(2) - i(3)
      Seq(sum1 + sum2, sum1 - sum2, diff1 + diff2, diff1 - diff2)
    }
  }

  /**
   * Returns the SPL representation of this module, which in this case is a DFT of size 4.
   *
   * @return An SPL representation for the DFT4 operation.
   */
  override def spl: SPL[T] = {
    // Ensure we have an implicit ComplexOps instance
    //implicit val ops: ComplexOps[T] = implicitly[ComplexOps[T]]
    DFT4[T]()  // Create a DFT4 instance with the appropriate implicit parameter
  }
}

/**
 * Butterfly class for Radix-8 FFT computation.
 *
 * @param k Logarithm of the input size (i.e., the streaming width).
 * @tparam T The data type for the input signals, parameterized with hardware type.
 */
case class Butterfly8[T: HW: ComplexOps](override val k: Int) extends AcyclicStreamingModule[T](0, k) {
  override def toString: String = "F8"

  /**
   * Implements the butterfly operation for Radix-8.
   *
   * @param inputs A sequence of input signals.
   * @return A sequence of output signals after performing the Radix-8 butterfly computation.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Butterfly8: Implementing with ${inputs.size} inputs.")
    
    val inputSize = 1 << 3  // Calculate expected input size as 2^3

    // Ensure the number of inputs is a multiple of inputSize; if not, pad with zeros.
    val paddedInputs = if (inputs.size % inputSize != 0) {
      println(s"Warning: Input size is not a multiple of $inputSize, padding with zeros. Current size: ${inputs.size}")
      inputs ++ Seq.fill(inputSize - (inputs.size % inputSize))(inputs.head - inputs.head)  // Padding with zeros
    } else {
      inputs
    }

    // Process input groups of inputSize
    paddedInputs.grouped(inputSize).toSeq.flatMap { i =>
      // Perform butterfly computation on the group of inputSize
      val sum1 = i(0) + i(4)  // (x0 + x4)
      val sum2 = i(1) + i(5)  // (x1 + x5)
      val sum3 = i(2) + i(6)  // (x2 + x6)
      val sum4 = i(3) + i(7)  // (x3 + x7)
      val diff1 = i(0) - i(4) // (x0 - x4)
      val diff2 = i(1) - i(5) // (x1 - x5)
      val diff3 = i(2) - i(6) // (x2 - x6)
      val diff4 = i(3) - i(7) // (x3 - x7)

      // Generate outputs for the Radix-8 FFT butterfly
      Seq(
        sum1 + sum2,   // Output 1: (x0 + x4) + (x1 + x5)
        sum3 + sum4,   // Output 2: (x2 + x6) + (x3 + x7)
        sum1 - sum2,   // Output 3: (x0 + x4) - (x1 + x5)
        sum3 - sum4,   // Output 4: (x2 + x6) - (x3 + x7)
        diff1 + diff2, // Output 5: (x0 - x4) + (x1 - x5)
        diff3 + diff4, // Output 6: (x2 - x6) + (x3 - x7)
        diff1 - diff2, // Output 7: (x0 - x4) - (x1 - x5)
        diff3 - diff4  // Output 8: (x2 - x6) - (x3 - x7)
      )
    }
  }

  /**
   * Returns the SPL representation of this module, which in this case is a DFT of size 8.
   *
   * @return An SPL representation for the DFT8 operation.
   */
    override def spl: SPL[T] = {
    // Ensure we have an implicit ComplexOps instance
    //implicit val ops: ComplexOps[T] = implicitly[ComplexOps[T]]
    DFT8[T]()  // Create a DFT8 instance with the appropriate implicit parameter
  }
}
/**
 * Butterfly class for Radix-16 FFT computation.
 *
 * @param k Logarithm of the input size (i.e., the streaming width).
 * @tparam T The data type for the input signals, parameterized with hardware type.
 */
case class Butterfly16[T: HW: ComplexOps](override val k: Int) extends AcyclicStreamingModule[T](0, k) {
  override def toString: String = "F16"

  /**
   * Implements the butterfly operation for Radix-16.
   *
   * @param inputs A sequence of input signals.
   * @return A sequence of output signals after performing the Radix-16 butterfly computation.
   */
  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    println(s"Butterfly16: Implementing with ${inputs.size} inputs.")
    val inputSize = 1 << 4  // Calculate expected input size as 2^4
    
    // Ensure the number of inputs is a multiple of inputSize; if not, pad with zeros.
    val paddedInputs = if (inputs.size % inputSize != 0) {
      println(s"Warning: Input size is not a multiple of $inputSize, padding with zeros. Current size: ${inputs.size}")
      inputs ++ Seq.fill(inputSize - (inputs.size % inputSize))(inputs.head - inputs.head)  // Padding with zeros
    } else {
      inputs
    }

    // Process input groups of inputSize
    paddedInputs.grouped(inputSize).toSeq.flatMap { i =>
      println(s"Processing group: ${i.mkString(", ")}")

      // Perform butterfly computation on the group of inputSize
      val sum1 = i(0) + i(8)
      val sum2 = i(1) + i(9)
      val sum3 = i(2) + i(10)
      val sum4 = i(3) + i(11)
      val sum5 = i(4) + i(12)
      val sum6 = i(5) + i(13)
      val sum7 = i(6) + i(14)
      val sum8 = i(7) + i(15)

      val diff1 = i(0) - i(8)
      val diff2 = i(1) - i(9)
      val diff3 = i(2) - i(10)
      val diff4 = i(3) - i(11)
      val diff5 = i(4) - i(12)
      val diff6 = i(5) - i(13)
      val diff7 = i(6) - i(14)
      val diff8 = i(7) - i(15)

      Seq(
        sum1 + sum2, sum3 + sum4, sum5 + sum6, sum7 + sum8,
        sum1 - sum2, sum3 - sum4, sum5 - sum6, sum7 - sum8,
        diff1 + diff2, diff3 + diff4, diff5 + diff6, diff7 + diff8,
        diff1 - diff2, diff3 - diff4, diff5 - diff6, diff7 - diff8
      )
    }
  }

  /**
   * Returns the SPL representation of this module, which in this case is a DFT of size 16.
   */
  override def spl: SPL[T] = DFT16[T]()
}
