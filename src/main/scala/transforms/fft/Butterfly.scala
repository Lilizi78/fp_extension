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
import transforms.fft.{DFT2, DFT4, DFT8}

case class Butterfly[T: HW]() extends AcyclicStreamingModule[T](0, 1) {
  override def toString: String = "F2"

  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    inputs.grouped(2).toSeq.flatMap(i => Seq(i.head + i.last, i.head - i.last))
  }

  override def spl: SPL[T] = DFT2[T]()(implicitly[HW[T]].num)
}

case class Butterfly4[T: HW]() extends AcyclicStreamingModule[T](0, 1) {
  override def toString: String = "F4"

  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    inputs.grouped(4).toSeq.flatMap { i =>
      val sum1 = i(0) + i(1)
      val sum2 = i(2) + i(3)
      val diff1 = i(0) - i(1)
      val diff2 = i(2) - i(3)
      Seq(sum1 + sum2, sum1 - sum2, diff1 + diff2, diff1 - diff2)
    }
  }

  override def spl: SPL[T] = DFT4[T]()(implicitly[HW[T]].num)
}

case class Butterfly8[T: HW]() extends AcyclicStreamingModule[T](0, 1) {
  override def toString: String = "F8"

  override def implement(inputs: Seq[Sig[T]]): Seq[Sig[T]] = {
    inputs.grouped(8).toSeq.flatMap { i =>
      val sum1 = i(0) + i(4)
      val sum2 = i(1) + i(5)
      val sum3 = i(2) + i(6)
      val sum4 = i(3) + i(7)
      val diff1 = i(0) - i(4)
      val diff2 = i(1) - i(5)
      val diff3 = i(2) - i(6)
      val diff4 = i(3) - i(7)
      Seq(sum1 + sum2, sum3 + sum4, sum1 - sum2, sum3 - sum4,
        diff1 + diff2, diff3 + diff4, diff1 - diff2, diff3 - diff4)
    }
  }

  override def spl: SPL[T] = DFT8[T]()(implicitly[HW[T]].num)
}
