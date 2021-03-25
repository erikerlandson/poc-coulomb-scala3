package coulomb.infra

import scala.quoted.*
import scala.annotation.targetName

object rational:
    trait /[L, R]

    class Rational private (val n: BigInt, val d: BigInt) extends Serializable:

        override def toString: String = s"$n/$d"

        @targetName("add")
        def +(rhs: Rational): Rational =
            Rational.apply((n * rhs.d) + (rhs.n * d), d * rhs.d)

        @targetName("sub")
        def -(rhs: Rational): Rational =
            Rational.apply((n * rhs.d) - (rhs.n * d), d * rhs.d)

        @targetName("mul")
        def *(rhs: Rational): Rational =
            Rational.apply(n * rhs.n, d * rhs.d)

        @targetName("div")
        def /(rhs: Rational): Rational =
            Rational.apply(n * rhs.d, d * rhs.n)

        @targetName("neg")
        def unary_- : Rational =
            Rational.apply(-n, d)

/*
        def pow(e: Rational): Rational =
            if (e.n == 0) then
                Rational.apply(1, 1)
            else if ((e.n == 1) && (e.d == 1)) then
                Rational.apply(this)
            else
                Rational.apply(1, 1)
*/

    end Rational

    object Rational:
        import scala.math.*

        def integerRoot(v: BigInt, k: BigInt): Boolean =
            val t = scala.math.pow(v.toDouble, k.toDouble)
            t == scala.math.rint(t)

        def apply(n: BigInt, d: BigInt): Rational =
            require(d != 0)
            if (n == 0)
                new Rational(0, 1)
            else if (d < 0) then
                apply(-n, -d)
            else
                val g = n.gcd(d)
                new Rational(n / g, d / g)

        inline def apply(r: Rational): Rational = apply(r.n, r.d)

        inline def apply(v: Int): Rational = apply(v, 1)
        inline def apply(v: Long): Rational = apply(v, 1)
        inline def apply(v: Float): Rational = apply(v.toDouble)
        def apply(v: Double): Rational =
            if (abs(v) == 0.0) then
                new Rational(0, 1)
            else
                val e = 15 - (floor(log10(abs(v))).toInt + 1)
                val (np10, dp10) = if (e < 0) then (-e, 0) else (0, e)
                val vi = v * pow(10, e)
                val n = BigInt(vi.toLong) * BigInt(10).pow(np10)
                val d = BigInt(10).pow(dp10)
                new Rational(n, d)
    end Rational

    extension(r: Rational)
        inline def toInt: Int = r.toDouble.toInt
        inline def toLong: Long = r.toDouble.toLong
        inline def toFloat: Float = r.toDouble.toFloat
        inline def toDouble: Double = r.n.toDouble / r.d.toDouble

end rational
