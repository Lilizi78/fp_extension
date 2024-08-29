package transforms.fft

import ir.rtl.hardwaretype.HW
import ir.rtl.{AcyclicStreamingModule, StreamingModule, RAMControl}
import ir.rtl.signals.Sig
import ir.spl.{Repeatable, SPL}
import linalg.Fields.Complex

class DFT8[T](implicit val num: Numeric[T])
    extends SPL[T](3) 
    with Repeatable[T] {
  
  override def eval(inputs: Seq[T], set: Int): Seq[T] = inputs
    .grouped(8)
    .toSeq
    .flatMap(i => {
      val sum1 = num.plus(i(0), i(4))
      val sum2 = num.plus(i(1), i(5))
      val sum3 = num.plus(i(2), i(6))
      val sum4 = num.plus(i(3), i(7))
      val diff1 = num.minus(i(0), i(4))
      val diff2 = num.minus(i(1), i(5))
      val diff3 = num.minus(i(2), i(6))
      val diff4 = num.minus(i(3), i(7))
      Seq(num.plus(sum1, sum2), num.plus(sum3, sum4), num.minus(sum1, sum2), num.minus(sum3, sum4),
          num.plus(diff1, diff2), num.plus(diff3, diff4), num.minus(diff1, diff2), num.minus(diff3, diff4))
    })

  override def stream(k: Int, control: RAMControl)(implicit
      hw: HW[T]
  ): AcyclicStreamingModule[T] = {
    println(s"DFT8 stream method called with k=$k")
    require(k == 2)
    Butterfly8[T]()
  }
}

object DFT8 {
  def apply[T: Numeric]() = new DFT8[T]()
  def unapply[T](arg: SPL[T]): Boolean = arg match {
    case _: DFT8[T] => true
    case _          => false
  }
}
