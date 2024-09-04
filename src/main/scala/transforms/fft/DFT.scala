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

import ir.rtl.hardwaretype.{ComplexHW, FixedPoint, HW}
import ir.rtl.{AcyclicStreamingModule, StreamingModule}
import ir.spl._
import linalg.Fields.{Complex, F2, F4, F8, F16}
import linalg.Matrix
import transforms.perm
import transforms.perm.LinearPerm

/**
 * Object containing methods for computing the DFT (Discrete Fourier Transform)
 * using different FFT (Fast Fourier Transform) algorithms and decomposition strategies.
 */
object DFT:

  /**
   * Function Q computes a permutation matrix Q used in FFT decompositions.
   * 
   * @param n Logarithm of the total size of the DFT transform (2^n points).
   * @param r Logarithm of the radix size for the FFT (2^r).
   * @param l Index of the current block being processed in the FFT decomposition.
   * @tparam T The type parameter representing the field type (e.g., Complex[Double]).
   * @return A LinearPerm object representing the permutation matrix Q for the FFT.
   */
  def Q[T](n: Int, r: Int, l: Int): SPL[T] = {
    println(s"Function Q: n=$n, r=$r, l=$l")
    
    // Construct permutation matrices for FFT
    val mat1 = Matrix.identity[F2](r * l) oplus LinearPerm.Lmat(n - r * (l + 1), n - r * l)
    val mat2 = Matrix.identity[F2](r * (l + 1)) oplus LinearPerm.Lmat(r, n - r * (l + 1))
    val mat = mat1 * mat2

    println(s"Generated Matrix for Q: $mat")
    
    // Return the permutation matrix as an SPL (Stream Processing Language) object
    LinearPerm[T](mat)
  }

  /**
   * Recursive function to compute the Cooley-Tukey DFT (CTDFT) using a divide-and-conquer strategy.
   * 
   * @param n Logarithm of the total size of the DFT transform (2^n points).
   * @param r Logarithm of the radix size for the FFT (2^r).
   * @return An SPL object representing the CTDFT computation as a combination of smaller FFT modules.
   */
  def CTDFT(n: Int, r: Int): SPL[Complex[Double]] = {
    println(s"Function CTDFT: n=$n, r=$r")
    assert(n % r == 0, s"n ($n) must be divisible by r ($r)")

    if (n == r) {
      // Terminate recursion with the base DFT module (e.g., DFT4 for Radix-4)
      println(s"Terminating recursion at n=$n, r=$r with DFT${1 << r}")
      r match {
        case 1 => DFT2()  // Base case for Radix-2
        case 2 => DFT4()  // Base case for Radix-4
        case 3 => DFT8()  // Base case for Radix-8
        case 4 => DFT16() // Base case for Radix-16
        case _ => throw new IllegalArgumentException(s"Unsupported radix: r=$r")
      }
    } else {
      // Expand the CTDFT using recursive decomposition and permutations
      println(s"Expanding CTDFT for n=$n, r=$r")
      
      val lmat = LinearPerm.Lmat(r, n)  // Left permutation matrix
      val rmat = LinearPerm.Rmat(r, n)  // Right permutation matrix
      
      // Recursive expansion using products of tensor, diagonal, and Q matrices
      val expandedProduct = Product(n / r)(l => {
        println(s"Inside Product loop with l=$l, n/r=${n/r}, r=$r")
        
        val tensor = ITensor(n - r, CTDFT(r, r))  // Tensor product of smaller DFTs
        val diag = DiagE(n, r, l)  // Diagonal matrix with twiddle factors
        val q = Q[Complex[Double]](n, r, l)  // Permutation matrix Q
        
        println(s"Tensor: $tensor, DiagE: $diag, Q: $q")
        tensor * diag * q  // Combine the matrices to form the expanded product
      })

      // Apply the permutation matrices to complete the recursive step
      LinearPerm(lmat) * expandedProduct * LinearPerm(rmat)
    }
  }

  /**
   * Function to compute the complex exponential factor (twiddle factor) for FFT.
   * 
   * @param n Logarithm of the total size of the DFT transform (2^n points).
   * @param pow The power to which the base root of unity is raised.
   * @return A complex number representing e^(-2πi * pow / 2^n).
   */
  def omega(n: Int, pow: Int): Complex[Double] = {
    // Calculate the angle for the complex exponential
    val N = 1 << n  // Compute 2^n
    val angle = -2 * Math.PI * pow / N
    Complex(Math.cos(angle), Math.sin(angle))  // Return the complex exponential
  }

  /**
   * Function implementing the Pease FFT algorithm using linear permutations and diagonal matrices.
   * 
   * @param n Logarithm of the total size of the DFT transform (2^n points).
   * @param r Logarithm of the radix size for the FFT (2^r).
   * @return An SPL object representing the Pease FFT computation.
   */
  def Pease(n: Int, r: Int): SPL[Complex[Double]] =
    println(s"Function Pease: n=$n, r=$r")
    assert(n % r == 0, "n must be divisible by r")
    
    if n == r then
      // Base cases for different radices
      r match {
        case 1 => DFT2()
        case 2 => DFT4()
        case 3 => DFT8()
        case 4 => DFT16()
        case _ => throw new IllegalArgumentException(s"Unsupported radix: $r")
      }
    else
      // Construct the Pease FFT using permutations and diagonal matrices
      LinearPerm(LinearPerm.Rmat(r, n)) * Product(n / r)(l =>
        DiagC(n, r, n / r - l - 1) * ITensor(n - r, CTDFT(r, r)) * LinearPerm(LinearPerm.Lmat(r, n).inverse)
      )

  /**
   * Function implementing the Iterative Pease FFT algorithm.
   * 
   * @param n Logarithm of the total size of the DFT transform (2^n points).
   * @param r Logarithm of the radix size for the FFT (2^r).
   * @return An SPL object representing the Iterative Pease FFT computation.
   */
  def ItPease(n: Int, r: Int): SPL[Complex[Double]] =
    println(s"Function ItPease: n=$n, r=$r")
    assert(n % r == 0, "n must be divisible by r")
    
    if n == r then
      r match {
        case 1 => DFT2()
        case 2 => DFT4()
        case 3 => DFT8()
        case 4 => DFT16()
        case _ => throw new IllegalArgumentException(s"Unsupported radix: $r")
      }
    else
      // Construct the Iterative Pease FFT using streaming diagonals and tensors
      LinearPerm(LinearPerm.Rmat(r, n)) * ItProduct(n / r,
        StreamDiagC(n, r) * ITensor(n - r, CTDFT(r, r)) * LinearPerm(LinearPerm.Lmat(r, n).inverse)
      )

  /**
   * Function implementing the Fused Iterative Pease FFT algorithm for optimized performance.
   * 
   * @param n Logarithm of the total size of the DFT transform (2^n points).
   * @param r Logarithm of the radix size for the FFT (2^r).
   * @return An SPL object representing the Fused Iterative Pease FFT computation.
   */
  def ItPeaseFused(n: Int, r: Int): SPL[Complex[Double]] =
    println(s"Function ItPeaseFused: n=$n, r=$r")
    assert(n % r == 0, "n must be divisible by r")
    
    if n == r then
      r match {
        case 1 => DFT2()
        case 2 => DFT4()
        case 3 => DFT8()
        case 4 => DFT16()
        case _ => throw new IllegalArgumentException(s"Unsupported radix: $r")
      }
    else
      // Use fused permutations and diagonals for further optimization
      ItProduct(n / r + 1,
        perm.LinearPerm(Seq.fill(n / r)(LinearPerm.Lmat(r, n).inverse) :+ LinearPerm.Rmat(r, n)),
        Some(StreamDiagC(n, r) * ITensor(n - r, CTDFT(r, r)))
      )

/**
 * Dummy DFT module for graph representation purposes.
 * 
 * @param t Logarithm of the transform size.
 * @param k Logarithm of the streaming width of the implementation.
 */
case class DFT(override val t: Int, override val k: Int) extends AcyclicStreamingModule(t, k)(using ComplexHW(FixedPoint(8, 8))):

  /**
   * Implement method left as a placeholder for specific DFT computations.
   * 
   * @param inputs A sequence of input signals.
   * @return A sequence of output signals after performing the DFT computation.
   */
  override def implement(inputs: Seq[ir.rtl.signals.Sig[Complex[Double]]]) = ???

  /**
   * Returns the SPL representation of the DFT using the CTDFT function.
   */
  override def spl = DFT.CTDFT(t + k, 1)
