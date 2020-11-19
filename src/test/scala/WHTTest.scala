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

import SB.HW.FixedPoint
import SPL.WHT.WHT
import StreamingModule.StreamingModule
import linalg.{Matrix, Vec}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties, Shrink}

object WHTTest extends Properties("WHT")  {
  implicit def shrinkSB[T]: Shrink[StreamingModule[T]] = Shrink.withLazyList {
    case StreamingModule.Product(factors) => factors.indices.to(LazyList).map(i => StreamingModule.Product[T](factors.take(i) ++ factors.drop(i + 1)))
    case SB.Product(factors) => factors.indices.to(LazyList).map(i => SB.Product[T](factors.take(i) ++ factors.drop(i + 1)))
    case SB.ITensor(r, factor, k) if k > factor.n => (1 to k - factor.n).to(LazyList).map(i => SB.ITensor(r - i, factor, k - i))
    case StreamingModule.ItProduct(r, factor: SB.Product[T], endLoop) => shrinkSB[T].shrink(factor).to(LazyList).map(f => StreamingModule.ItProduct(r, f, endLoop))
    case StreamingModule.ItProduct(r, factor, endLoop) => (1 until r).reverse.to(LazyList).map(i => StreamingModule.ItProduct(i, factor, endLoop))
    case input => (1 until input.k).reverse.to(LazyList).map(k => input.spl.stream(k)(input.hw))
  }


  property("WHT conforms to the definition")=forAll (Gen.choose(1,10),Gen.oneOf(true,false)){(n,dp)=>
      val sb = WHT[Double](n, 1,dp) // Temporal(Vector(Vec.fromInt(2, 3)), Vector(Matrix[F2](2, 2, Vector(1, 1, 1, 0))))(Unsigned(16))
      val res=(0 until (1<<n)).map (j=> Vec(sb.eval(
        Seq.tabulate(1 << n)(i => if (i == j) 1.0 else 0.0), 0
      ).toVector)).reduce[Matrix[Double]](_ :: _)
      val wht=Matrix.tabulate[Double](1<<n,1<<n)((i,j)=>if((Vec.fromInt(n,i) scalar Vec.fromInt(n,j)).value) -1 else 1)
      (res-wht).norm==0
    }


  val genSteady: Gen[StreamingModule[Double]] = for {
    t <- Gen.choose(1, 2)
    k <- Gen.choose(1, 2)
    dp <- Gen.oneOf(true,false)
  } yield WHT[Double](t + k, 1,dp).stream(k)(FixedPoint(16, 0))
  property("CTWHT")=  forAll(genSteady) { sb:StreamingModule[Double] => sb.test() match{
        case Some(value) if value<0.01 => true
        case _ => false
      }}

  property("PeaseWHT conforms to the definition")=forAll(for {
    n <- Gen.choose(2,10)
    r <- Gen.choose(1, n-1)
    if n % r == 0
    dp <- Gen.oneOf(true,false)
  } yield (n,r,dp)) { case (n,r,dp) =>
      val sb = WHT.Pease[Double](n, r,dp) // Temporal(Vector(Vec.fromInt(2, 3)), Vector(Matrix[F2](2, 2, Vector(1, 1, 1, 0))))(Unsigned(16))
      val res = (0 until (1 << n)).map(j => Vec(sb.eval(
        Seq.tabulate(1 << n)(i => if (i == j) 1.0 else 0.0), 0
      ).toVector)).reduce[Matrix[Double]](_ :: _)
      val wht = Matrix.tabulate[Double](1 << n, 1 << n)((i, j) => if ((Vec.fromInt(n, i) scalar Vec.fromInt(n, j)).value) -1 else 1)
      (res - wht).norm == 0
    }


  val peaseWHT: Gen[StreamingModule[Double]] = for {
    t <- Gen.choose(1, 2)
    k <- Gen.choose(1, 2)
    n = t + k
    r <- Gen.choose(1, n)
    if n % r == 0
    dp <- Gen.oneOf(true,false)
  } yield WHT.Pease[Double](n, r,dp).stream(k)(FixedPoint(16, 0))
  property("PeaseWHT") =    forAll(peaseWHT) { sb: StreamingModule[Double] =>
      sb.test() match {
        case Some(value) if value < 0.01 => true
        case _ => false
      }
    }

  val itpeaseWHT: Gen[StreamingModule[Double]] = for {
    t <- Gen.choose(1, 2)
    k <- Gen.choose(1, 2)
    n = t + k
    r <- Gen.choose(1, n)
    if n % r == 0
  } yield WHT.ItPease[Double](n, r).stream(k)(FixedPoint(16, 0))
  property("ItPeaseWHT") =

    forAll(itpeaseWHT) { sb: StreamingModule[Double] =>
      sb.test() match {
        case Some(value) if value < 0.01 => true
        case _ => false
      }
    }
}