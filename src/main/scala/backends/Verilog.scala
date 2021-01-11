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

package backends
import ir.rtl._
import scala.collection.mutable
import scala.annotation.tailrec

object Verilog {
  extension (mod:Module){
    final def toVerilog: String = {
      val names = mutable.AnyRefMap[Component, String]()
      val toImplement = mutable.Queue[Component]()
      toImplement.enqueueAll(mod.outputs)

      implicit class CompName(cp:Component) {
        @tailrec
        final def id:String = cp match {
          case comp: Input => comp.name
          case comp: Output => comp.name
          case comp: Wire => comp.input.id
          case comp: Const => s"${comp.size}'d${comp.value}"
          case _ => if (!(names contains cp)) {
            names(cp) = s"s${names.size + 1}"
            toImplement.enqueue(cp)
          }
            names(cp)
        }
      }
      val combinatorial = new mutable.StringBuilder
      val sequential = new mutable.StringBuilder
      val declarations = new mutable.StringBuilder

      def addDec(line: String) = declarations ++= "  " ++= line ++= "\n"

      def addSeq(line: String*): Unit = line.foreach(sequential ++= "      " ++= _ ++= "\n")

      def addComb(line: String*): Unit = line.foreach(combinatorial ++= "  " ++= _ ++= "\n")

      //def addComb(line: Vector[String]) = line.foreach(combinatorial ++= "  " ++= _ ++= "\n")
      while (toImplement.nonEmpty) {
        val cur = toImplement.dequeue()
        cur match {
          case _: Output | _: Input | _: Const | _: Wire =>
          case _: Register | _: Mux | _: RAMRd => addDec(s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${cur.id};")
          case cur:RAMWr => addDec(s"reg ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${cur.id} [${(1 << cur.wrAddress.size) - 1}:0]; // synthesis attribute ram_style of ${cur.id} is block")
          case _ => addDec(s"wire ${if (cur.size != 1) s"[${cur.size - 1}:0] " else ""}${cur.id};")
        }
        //if (cur.description != "") addComb(s"// ${cur.description}")
        cur match {
          case cur:Output => addComb(s"assign ${cur.id} = ${cur.input.id};")
          case cur:Plus => addComb(s"assign ${cur.id} = ${cur.terms.map(_.id).mkString(" + ")};")
          case cur:Minus => addComb(s"assign ${cur.id} = ${cur.lhs.id} - ${cur.rhs.id};")
          case cur:Times => addComb(s"assign ${cur.id} = $$signed(${cur.lhs.id}) * $$signed(${cur.rhs.id});")
          case cur:And => addComb(s"assign ${cur.id} = ${cur.terms.map(_.id).mkString(" & ")};")
          case cur:Xor => addComb(s"assign ${cur.id} = ${cur.inputs.map(_.id).mkString(" ^ ")};")
          case cur: Or => addComb(s"assign ${cur.id} = ${cur.inputs.map(_.id).mkString(" | ")};")
          case cur:Equals => addComb(s"assign ${cur.id} = ${cur.lhs.id} == ${cur.rhs.id};")
          case cur:Not => addComb(s"assign ${cur.id} = ~${cur.input.id};")
          case cur:Mux =>
            addComb("always @(*)")
            addComb(s"  case(${cur.address.id})")
            cur.inputs.zipWithIndex.foreach { case (in, i) => addComb(s"    ${if (i == cur.inputs.size - 1 && ((1 << cur.address.size) != cur.inputs.size)) "default" else i}: ${cur.id} <= ${in.id};") }
            addComb("  endcase")
          case cur:Concat => addComb(s"assign ${cur.id} = {${cur.inputs.map(_.id).mkString(", ")}};")
          case cur:Tap => addComb(s"assign ${cur.id} = ${cur.input.id}[${if (cur.range.size > 1) s"${cur.range.last}:" else ""}${cur.range.start}];")
          case cur:Register => addSeq(s"${cur.id} <= ${cur.input.id};")
          case cur:RAMWr => addSeq(s"${cur.id} [${cur.wrAddress.id}] <= ${cur.input.id};")
          case cur:RAMRd => addSeq(s"${cur.id} <= ${cur.mem.id} [${cur.rdAddress.id}];")
          case cur:Extern => addComb(s"${cur.module} ext_${cur.id}(${cur.inputs.map{case (name, comp)=>s".$name(${comp.id}), "}.mkString}.${cur.outputName}(${cur.id}));")
            mod.dependencies.add(cur.filename)
          case _ =>
        }
      }

      var result = new StringBuilder
      result ++= s"module ${mod.name}(input clk,\n"
      result ++= mod.inputs.map(s => s"  input ${if (s.size != 1) s"[${s.size - 1}:0] " else ""}${s.id},\n").mkString("")
      result ++= mod.outputs.map(s => s"  output ${if (s.size != 1) s"[${s.size - 1}:0] " else ""}${s.id}").mkString(",\n")
      result ++= ");\n\n"
      result ++= declarations
      result ++= combinatorial
      result ++= "  always @(posedge clk)\n"
      result ++= "    begin\n"
      result ++= sequential
      result ++= "    end\n"
      result ++= "endmodule\n"
      result.toString()
    }
  }
  extension [U](sm:StreamingModule[U]) {
    def getTestBench(repeat: Int = 2, addedGap: Int = 0): String = {

      val input = sm.testBenchInput(repeat)

      //val input = Vector.tabulate(repeat)(set => Vector.tabulate[Int](N)(i => i * 100 + set * 1000))
      //val input = Vector.tabulate(repeat)(set => Vector.tabulate[BigInt](N)(i => 0))


      var res = new StringBuilder
      res ++= "module test;\n"
      res ++= "    reg clk,rst,next;\n"
      sm.dataInputs.foreach(res ++= "    reg [" ++= (sm.busSize - 1).toString ++= ":0] " ++= _.name ++= ";\n")
      res ++= "    wire next_out;\n"
      sm.dataOutputs.foreach(res ++= "    wire [" ++= (sm.busSize - 1).toString ++= ":0] " ++= _.name ++= ";\n")
      res ++= "\n"
      res ++= " //Clock\n"
      res ++= "    always\n"
      res ++= "      begin\n"
      res ++= "        clk <= 0;#50;\n"
      res ++= "        clk <= 1;#50;\n"
      res ++= "      end\n"
      res ++= "\n"
      res ++= "//inputs\n"
      res ++= "    initial\n"
      res ++= "      begin\n"
      res ++= "        @(posedge clk);\n"
      res ++= "        next <= 0;\n"
      (0 to (sm.latency - sm.nextAt + sm.T)).foreach(_ => res ++= "        @(posedge clk);\n")
      res ++= "        rst <= 1;\n"
      res ++= "        @(posedge clk);\n"
      res ++= "        @(posedge clk);\n"
      res ++= "        rst <= 0;\n"
      (Math.min(sm.nextAt, 0) until Math.max((sm.T + sm.minGap + addedGap) * repeat, (sm.T + sm.minGap + addedGap) * (repeat - 1) + sm.nextAt + 4)).foreach(cycle => {
        res ++= "        @(posedge clk); //cycle " ++= cycle.toString ++= "\n"
        if ((cycle - sm.nextAt) >= 0 && (cycle - sm.nextAt) % (sm.T + sm.minGap + addedGap) == 0 && (cycle - sm.nextAt) / (sm.T + sm.minGap + addedGap) < repeat)
          res ++= "        next <= 1;\n"
        if ((cycle - sm.nextAt + 1) >= 0 && (cycle - sm.nextAt) % (sm.T + sm.minGap + addedGap) == 1 && (cycle - sm.nextAt) / (sm.T + sm.minGap + addedGap) < repeat)
          res ++= "        next <= 0;\n"
        val set = cycle / (sm.T + sm.minGap + addedGap)
        val c = cycle % (sm.T + sm.minGap + addedGap)
        if (set < repeat && cycle >= 0 && c < sm.T) {
          if (c == 0)
            res ++= "        //dataset " + set + " enters.\n"
          sm.dataInputs.zipWithIndex.foreach(i => res ++= "        " ++= i._1.name ++= " <= " ++= sm.hw.size.toString ++= "'d" ++= input(set * sm.N + c * sm.K + i._2).toString ++= ";\n")
        }
      })

      res ++= "      end\n"
      res ++= "    initial\n"
      res ++= "      begin\n"
      res ++= "        @(posedge next_out);//#100;\n"
      res ++= "        #50;\n"
      //if (check) {
      val output = input.grouped(sm.N).toSeq.zipWithIndex.flatMap { case (input, set) => sm.eval(input, set) }
      (0 until repeat).foreach(r => {
        (0 until sm.T).foreach(c => {
          (0 until sm.K).foreach(i => {
            res ++= "        $display(\"output" ++= (r * sm.T * sm.K + c * sm.K + i).toString ++= ": %0d (expected: " ++= output(r * sm.N + c * sm.K + i).toString ++= ")\"," ++= sm.dataOutputs(i).name ++= ");\n"
            //res ++= "        if(^" ++= inputs(i).toString ++= "===1'bX) $finish();\n"
            //res ++= "        errorSum = errorSum + (" ++= output(r)(c * T + i).toString ++= "-" ++= outputs(i).name + ")*(" ++= output(r)(c * K + i).toString ++= "-" ++= outputs(i).name + ");\n"

          })

          res ++= "        #100;\n"
        })

        res ++= "        #" + (100 * (sm.minGap + addedGap)) + "; //gap\n"
      })
      //}
      /*else {
      (0 until repeat).foreach(r => {
        (0 until (T)).foreach(c => {
          (0 until K).foreach(i => {
            res ++= "        $display(\"output" ++= (c * T + i).toString ++= ": %d \"," ++= dataOutputs(i).name ++= ");\n"
          })
          res ++= "        #100;\n"
        })
        res ++= "        #" + (100 * minGap) + "; //gap\n"
      })
    }*/
      res ++= "        $display(\"Success.\");\n"
      res ++= "        $finish();\n"
      res ++= "      end\n"
      res ++= "      " ++= sm.name ++= " uut(clk,rst,next," ++= (0 until sm.K).map(i => sm.dataInputs(i).name).mkString(",") ++= ",next_out," ++= (0 until sm.K).map(i => sm.dataOutputs(i).name).mkString(",") ++= ");\n"
      res ++= "endmodule\n"
      res.toString
    }

  }
}
