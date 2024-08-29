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
import linalg.Fields.{Complex, F2, F4, F8}
import linalg.Matrix
import transforms.perm
import transforms.perm.LinearPerm

object DFT:
  def Q[T](n: Int, r: Int, l: Int): SPL[T] = 
    println(s"Function Q: n=$n, r=$r, l=$l")
    val mat1 = Matrix.identity[F2](r * l) oplus LinearPerm.Lmat(n - r * (l + 1), n - r * l)
    val mat2 = Matrix.identity[F2](r * (l + 1)) oplus LinearPerm.Lmat(r, n - r * (l + 1))
    val mat = mat1 * mat2
    LinearPerm[T](mat)

  def CTDFT(n: Int, r: Int): SPL[Complex[Double]] = {
      println(s"Function CTDFT: n=$n, r=$r")
      assert(n % r == 0)
      if (n == r) {
          println(s"Terminating recursion at n=$n, r=$r with DFT${1<<r}")
          r match {
              case 1 => DFT2()
              case 2 => DFT4()
              case 3 => DFT8()
              // Add more cases for other radices if needed
              case _ => throw new IllegalArgumentException(s"Unsupported radix: r=$r")
          }
          //println(s"~~Terminating recursion at n=$n, r=$r with DFT${r}")
      } else {
          println(s"Expanding CTDFT for n=$n, r=$r")
          LinearPerm(LinearPerm.Lmat(r, n)) * Product(n / r)(l => {
              println(s"Inside Product loop with l=$l")
              ITensor(n - r, CTDFT(r, r)) * DiagE(n, r, l) * Q(n, r, l)
          }) * LinearPerm(LinearPerm.Rmat(r, n))
      }
  }


  def omega(n: Int, pow: Int): Complex[Double] =
    //println(s"Function omega: n=$n, pow=$pow") 
    if pow % (1 << n) == 0 then
      Complex(1)
    else if 2 * (pow % (1 << n)) == (1 << n) then
      Complex(-1)
    else if 4 * (pow % (1 << n)) == (1 << n) then
      Complex(0,-1)
    else if 4 * (pow % (1 << n)) == 3 * (1 << n) then
      Complex(0,1)
    else 
      val angle = -2 * Math.PI * pow / (1 << n)
      Complex(Math.cos(angle), Math.sin(angle))
    
  def Pease(n: Int, r: Int): SPL[Complex[Double]] =
    println(s"Function Pease: n=$n, r=$r") 
    assert(n%r==0)
    if n == r then
      r match {
        case 1 => DFT2()
        case 2 => DFT4()
        case 3 => DFT8()
        case _ => throw new IllegalArgumentException(s"Unsupported radix: $r")
      }
    else
      LinearPerm(LinearPerm.Rmat(r, n)) * Product(n / r)(l => DiagC(n, r, n/r-l-1) * ITensor(n-r, CTDFT(r, r)) * LinearPerm(LinearPerm.Lmat(r, n).inverse))

  def ItPease(n: Int, r: Int): SPL[Complex[Double]] =
    println(s"Function ItPease: n=$n, r=$r") 
    assert(n % r == 0)
    if n == r then
      r match {
        case 1 => DFT2()
        case 2 => DFT4()
        case 3 => DFT8()
        case _ => throw new IllegalArgumentException(s"Unsupported radix: $r")
      }
    else
      LinearPerm(LinearPerm.Rmat(r, n)) * ItProduct(n / r, StreamDiagC(n, r) * ITensor(n - r, CTDFT(r, r)) * LinearPerm(LinearPerm.Lmat(r, n).inverse))

  def ItPeaseFused(n: Int, r: Int): SPL[Complex[Double]] =
    println(s"Function ItPeaseFused: n=$n, r=$r")
    assert(n % r == 0)
    if n == r then
      r match {
        case 1 => DFT2()
        case 2 => DFT4()
        case 3 => DFT8()
        case _ => throw new IllegalArgumentException(s"Unsupported radix: $r")
      }
    else
      ItProduct(n / r + 1, perm.LinearPerm(Seq.fill(n / r)(LinearPerm.Lmat(r, n).inverse) :+ LinearPerm.Rmat(r, n)), Some(StreamDiagC(n, r) * ITensor(n - r, CTDFT(r, r))))
  //def stream(n: Int, r: Int, k: Int, hw: HW[Complex[Double]], dualPorted: Boolean): StreamingModule[Complex[Double]] = CTDFT(n, r).stream(k)(hw)


/** Dummy module used for representation in graphs */
case class DFT(override val t:Int, override val k:Int) extends AcyclicStreamingModule(t,k)(using ComplexHW(FixedPoint(8,8))):
  
  override def implement(inputs: Seq[ir.rtl.signals.Sig[Complex[Double]]]) = ???

  override def spl = DFT.CTDFT(t + k, 1)
  
