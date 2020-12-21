/*
 *    _____ ______          SGen - A Generator of Streaming Hardware
 *   / ___// ____/__  ____  Department of Computer Science, ETH Zurich, Switzerland
 *   \__ \/ / __/ _ \/ __ \
 *  ___/ / /_/ /  __/ / / / Copyright (C) 2020 François Serre (serref@inf.ethz.ch)
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

package SPL

import AcyclicStreamingModule.HardwareType.HW
import AcyclicStreamingModule.SLP.RAMControl
import StreamingModule.StreamingModule

case class ItProduct[T](r: Int, factor: SPL[T], endLoopOpt: Option[SPL[T]] = None) extends SPL[T](factor.n) {
  val endLoop: SPL[T] = endLoopOpt.getOrElse(Identity[T](n))

  override def eval(inputs: Seq[T], set: Int): Seq[T] = factor.eval((0 until (r - 1)).foldLeft(inputs)((endLoop * factor).eval), r - 1)

  override def stream(k: Int,control:RAMControl)(implicit hw: HW[T]): StreamingModule[T] = {
    require(control==RAMControl.Dual,"Iterative product requires a dual address control of memories.")
    StreamingModule.ItProduct(r, factor.stream(k,RAMControl.Dual), endLoopOpt.map(_.stream(k,RAMControl.Dual)))
  }
}

object ItProduct {
  //def apply[T](r: Int, factor: SPL[T]) = if (r == 1) factor else new ItProduct[T](r, factor)
}