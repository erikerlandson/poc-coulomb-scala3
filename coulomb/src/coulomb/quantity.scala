package coulomb

import scala.compiletime.{ summonInline, summonFrom }

trait Addable[V]:
    def plus(x: V, y: V): V
object Addable:
    given Addable[Int] with
        inline def plus(x: Int, y: Int): Int = x + y
    given Addable[Double] with
        inline def plus(x: Double, y: Double): Double = x + y
    given Addable[String] with
        inline def plus(x: String, y: String): String = x ++ y

trait Second

export quantity.Quantity as Quantity
export quantity.withUnit as withUnit

extension[V, U] (ql: Quantity[V, U])
    inline def +(qr: Quantity[V, U]): Quantity[V, U] =
        (summonInline[Addable[V]]).plus(ql.value, qr.value).withUnit[U]

extension[U] (ql: Quantity[Int, U])
    inline def +(qr: Quantity[Int, U]): Quantity[Int, U] =
        (summonInline[Addable[Int]]).plus(ql.value, qr.value).withUnit[U]

object quantity:
    opaque type Quantity[V, U] = V

    // The only two methods I need in scope of the opaque type
    // are a way to lift raw values into a Quantity
    // and a way to extract raw values from a quantity

    // lift
    object Quantity:
        def lift[V, U](v: V): Quantity[V, U] = v
        def lift[U](v: Int): Quantity[Int, U] = v
    end Quantity

    // extract
    extension[V, U](ql: Quantity[V, U])
        def value: V = ql
    extension[U](ql: Quantity[Int, U])
        def value: Int = ql

    extension[V](v: V)
        def withUnit[U]: Quantity[V, U] = v
    extension(v: Int)
        def withUnit[U]: Quantity[Int, U] = v

end quantity

object qvec:
    opaque type QVec[V, U] = Vector[V]
end qvec

object test:
    import coulomb.*

//    inline def addtest(q1: Quantity[Int, Second], q2: Quantity[Int, Second]): Quantity[Int, Second] =
//        q1 + (q2)

    // using summonInline and summonFrom makes 'inline' keyword 'viral', if
    // defining functions having type parameters
    inline def addTest[V, U](q1: Quantity[V, U], q2: Quantity[V, U]): Quantity[V, U] =
        q1 + q2

/*
    val lhs = 3.withUnit[Second]
    val rhs = 5.withUnit[Second]
    val zzz = lhs + rhs

    val www = addTest(lhs, rhs)
*/


    val t = 4.withUnit[Second]
end test
