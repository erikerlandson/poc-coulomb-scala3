package coulomb

/** Represents the product of two unit expressions L and R */
trait %*[L, R]

/** Represents the unit division L / R */
trait %/[L, R]

/** Represents raising unit expression B to integer power E */
trait %^[B, E]

/** type-level Rational */
trait /%[N, D]

trait SNil
trait %:[Head, Tail]

@deprecated("Unitless should be replaced by integer literal type '1'")
type Unitless = 1

export quantity.Quantity as Quantity
export quantity.withUnit as withUnit

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

trait UnitAdd[V1, U1, V2, U2]:
    type RV
    type RU
    def vadd(v1: V1, v2: V2): RV
object UnitAdd:
    transparent inline given UnitAdd[U1, U2](using coef: Coefficient[U2, U1]): UnitAdd[Double, U1, Double, U2] =
        new UnitAdd[Double, U1, Double, U2]:
            // just arbitrarily make return value type Float instead of Double
            type RV = Float
            type RU = U1
            val c = coef.coef.toDouble
            def vadd(v1: Double, v2: Double): Float = (v1 + (c * v2)).toFloat

extension[V1, U1](ql: Quantity[V1, U1])
    transparent inline def +[V2, U2](qr: Quantity[V2, U2])(using add: UnitAdd[V1, U1, V2, U2]): Quantity[add.RV, add.RU] =
        add.vadd(ql.value, qr.value).withUnit[add.RU]

/*
extension[V, U] (ql: Quantity[V, U])
    inline def +(qr: Quantity[V, U]): Quantity[V, U] =
        (summonInline[Addable[V]]).plus(ql.value, qr.value).withUnit[U]

extension[U] (ql: Quantity[Int, U])
    inline def +(qr: Quantity[Int, U]): Quantity[Int, U] =
        (ql.value + qr.value).withUnit[U]
*/

object conversion:
    inline given convValValUnit[V1, V2, U](using cv: scala.Conversion[V1, V2]):
            scala.Conversion[Quantity[V1, U], Quantity[V2, U]] =
        new scala.Conversion[Quantity[V1, U], Quantity[V2, U]]:
            def apply(q: Quantity[V1, U]): Quantity[V2, U] =
                (cv(q.value)).withUnit[U]

    inline given convDoubleUnitUnit[U1, U2](using conv: Coefficient[U1, U2]):
            scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]] =
        new scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]]:
            val c = conv.coef.toDouble
            def apply(q: Quantity[Double, U1]): Quantity[Double, U2] =
                (q.value * c).withUnit[U2]

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
    import coulomb.si.*

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

    //val r: Rational = 1
end test

object si:
    import coulomb.rational.Rational
    import coulomb.define.*

    trait Meter
    given BaseUnit[Meter] with
        val name = "meter"
        val abbv = "m"

    trait Kilogram
    given BaseUnit[Kilogram] with
        val name = "kilogram"
        val abbv = "kg"

    trait Second
    given BaseUnit[Second] with
        val name = "second"
        val abbv = "s"

    trait Liter
    given DerivedUnit[Liter, Meter %* Meter %* Meter] with
        val name = "liter"
        val abbv = "L"
        val coef = Rational(1, 1000)

    trait Hertz
    given DerivedUnit1[Hertz, 1 %/ Second] with
        val name = "Hertz"
        val abbv = "Hz"

    trait Newton
    given DerivedUnit1[Newton, Kilogram %* Meter %/ (Second %^ 2)] with
        val name = "Newton"
        val abbv = "N"

    trait Kilo
    given PrefixUnit[Kilo] with
        val name = "kilo"
        val abbv = "k"
        val coef = Rational(1000)

    trait Yard
    given yard: DerivedUnit[Yard, Meter] with
        val name = "yard"
        val abbv = "yd"
        val coef = Rational(9144, 10000)
