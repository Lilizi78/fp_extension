package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.rtl.signals.Sig
import ir.spl.{Repeatable, SPL}
import linalg.Fields.Complex

class DFT4[T](implicit val num: Numeric[T])
    extends SPL[T](2) 
    with Repeatable[T] {
  
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs
    .grouped(4)
    .toSeq
    .flatMap(i => {
      val sum1 = num.plus(i(0), i(1))
      val sum2 = num.plus(i(2), i(3))
      val diff1 = num.minus(i(0), i(1))
      val diff2 = num.minus(i(2), i(3))
      Seq(num.plus(sum1, sum2), num.minus(sum1, sum2), num.plus(diff1, diff2), num.minus(diff1, diff2))
    })

  override def stream(k: Int, control: RAMControl)(implicit
      hw: HW[T]
  ): AcyclicStreamingModule[T] = {
    println(s"DFT4 stream method called with k=$k, expected k=$k")
    require(k == 2, s"DFT4 requires k to be $k, but got k=$k")
    Butterfly4[T]()
  }
}

object DFT4 {
  def apply[T: Numeric]() = new DFT4[T]()
  def unapply[T](arg: SPL[T]): Boolean = arg match {
    case _: DFT4[T] => true
    case _          => false
  }
}
