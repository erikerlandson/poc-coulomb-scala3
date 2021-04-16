package coulomb

/** Represents the product of two unit expressions L and R */
trait %*[L, R]

/** Represents the unit division L / R */
trait %/[L, R]

/** Represents raising unit expression B to integer power E */
trait %^[B, E]

@deprecated("Unitless should be replaced by integer literal type '1'")
type Unitless = 1

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
        (ql.value + qr.value).withUnit[U]

object quantity:
    opaque type Quantity[V, U] = V

    // The only two methods I need in scope of the opaque type
    // are a way to lift raw values into a Quantity
    // and a way to extract raw values from a quantity

    trait Applier[U]:
        def apply[V](v: V): Quantity[V, U]
    object Applier:
        given [U]: Applier[U] = new Applier[U] { def apply[V](v: V): Quantity[V, U] = v } 

    // lift
    object Quantity:
        def apply[U](using a: Applier[U]) = a
        def apply[U](v: Int): Quantity[Int, U] = v
        def apply[U](v: Double): Quantity[Double, U] = v
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
    import coulomb.*, coulomb.rational.*
/*
    // using summonInline and summonFrom makes 'inline' keyword 'viral', if
    // defining functions having type parameters
    inline def addTest[V, U](q1: Quantity[V, U], q2: Quantity[V, U]): Quantity[V, U] =
        q1 + q2

    val lhs = 3.withUnit[Second]
    val rhs = 5.withUnit[Second]
    val zzz = lhs + rhs

    val zv = zzz.value

    val www = addTest(lhs, rhs)


    val t = 4.withUnit[Second]
*/
    val t1 = Quantity[Second](99)
    val t2 = Quantity[Second](99.9)
    val t3 = Quantity[Second]("foo")

    val r: Rational = 1
end test
