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


import ir.rtl.hardwaretype.{ComplexHW, FixedPoint, Flopoco, IEEE754, Unsigned, HW}
import transforms.perm.{Steady, SwitchArray, Temporal}
import ir.rtl._
import transforms.fft.DFT
import linalg.Fields.{Complex, F2}
import linalg.{Matrix, Vec}
import backends.DOT._
import backends.SVG._
import backends.Verilog._
import backends.Xsim._

import scala.sys.process._
import java.io.PrintWriter


/**
 * Script to generate all the elements used in the different websites.
 */
object GenerateWeb extends App{
  /*val uut = DFT.CTDFT(2,1).stream(1,RAMControl.Single)(ComplexHW(IEEE754(8,23))).asInstanceOf[SB[?]]
  uut.showGraph
  System.exit(0)*/
  
  
    transforms.fft.DFT.CTDFT(3,1).stream(3,RAMControl.Single)(ComplexHW(FixedPoint(16,0))).writeSVG("dft8.svg")
    transforms.fft.DFT.CTDFT(3,1).stream(2,RAMControl.Single)(ComplexHW(FixedPoint(16,0))).writeSVG("dft8s4.svg")
    transforms.fft.DFT.CTDFT(3,1).stream(1,RAMControl.Single)(ComplexHW(FixedPoint(16,0))).writeSVG("dft8s2.svg")
    for
      transform <- Vector("dft","dftcompact")
      n <- 1 to 15
      k <- 1 to Math.min(n,8)
      r <- 1 to k
      if n % r == 0
      hw <- Vector("char","short","int","long","half","float","double","bfloat16")
    do
      val name = s"$transform-$n-$k-$r-$hw"
      val rhw:HW[Double]=if(hw=="char")
        FixedPoint(4,4)
        else if(hw=="short")
        FixedPoint(8,8)
        else if(hw=="int")
        FixedPoint(16,16)
        else if(hw=="long")
        FixedPoint(32,32)
        else if(hw=="half")
        IEEE754(5,10)
        else if(hw=="float")
        IEEE754(8,23)
        else if(hw=="double")
        IEEE754(11,52)
        else 
        IEEE754(8,7)

      print(name + " - ")
      val uut=
        if transform=="dft" then
          DFT.CTDFT(n,r).stream(k,RAMControl.Single)(ComplexHW(rhw))
        else
          DFT.ItPeaseFused(n,r).stream(k,RAMControl.Dual)(ComplexHW(rhw))
      println(uut.test())


      Main.main(s"-zip -o $name.zip -testbench -n $n -k $k -r $r -hw complex $hw $transform".split(" "))
  
}
