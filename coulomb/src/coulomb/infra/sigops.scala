package coulomb.infra

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.any.*

trait SNil
trait %:[H, T]

trait Alloc[T]
def alloc[T] = new Alloc[T] {}

trait Addable[V]:
    def plus(x: V, y: V): V
  
given Addable[Int] with
    def plus(x: Int, y: Int): Int = x + y

trait Second

object demo:
    opaque type Quantity[V, U] = V

    object Quantity:
    end Quantity

    extension[V] (v: V)
      def withUnit[U]: Quantity[V, U] = v

    extension[V, U] (ql: Quantity[V, U])
        def value: V = ql
        def +(qr: Quantity[V, U])(using va: Addable[V]): Quantity[V, U] =
            va.plus(ql, qr)
end demo
