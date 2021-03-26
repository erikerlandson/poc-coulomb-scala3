package coulomb.infra

import scala.quoted.*
import scala.annotation.targetName

object rational:
    trait /[L, R]

    class Rational private (val n: BigInt, val d: BigInt) extends Serializable:

        override def toString: String =
            if (d == 1) s"$n" else s"$n/$d"

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

        def pow(e: Rational): Rational =
            if (n < 0) then
                require(e.d == 1) // negative can only be raised to integer power
                val p = (-this).pow(e)
                if (e.n % 2 == 0) then p else -p
            else if (e.n < 0) then
                Rational.apply(d, n).pow(-e)
            else if (e.n == 0) then
                require(n > 0) // 0.pow(0) is undefined
                Rational.apply(1, 1)
            else if (e.n == 1) then
                if (e.d == 1) then this else
                    // this >= 0, e is of form 1/k
                    (Rational.integerRoot(n, e.d), Rational.integerRoot(d, e.d)) match
                        case (Some(nr), Some(dr)) => Rational.apply(nr, dr)
                        case _ => Rational(scala.math.pow(this.toDouble, 1.0 / e.d.toDouble))
            else
                if (e.d == 1) then
                    Rational.apply(n.pow(e.n.toInt), d.pow(e.n.toInt))
                else
                    val p = Rational.apply(e.n, 1)
                    val r = Rational.apply(1, e.d)
                    this.pow(p).pow(r)

    end Rational

    object Rational:
        import scala.math.*

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
                apply(n, d)

        def integerRoot(v: BigInt, k: BigInt): Option[BigInt] =
            require(v >= 0 && k > 0)
            if k == 1 then Some(v)
            else
                val t = if k == 2 then sqrt(v.toDouble)
                else if k == 3 then cbrt(v.toDouble)
                else pow(v.toDouble, 1.0 / k.toDouble)
                if (t == rint(t)) then Some(BigInt(t.toLong)) else None
    end Rational

    extension(r: Rational)
        inline def toInt: Int = r.toDouble.toInt
        inline def toLong: Long = r.toDouble.toLong
        inline def toFloat: Float = r.toDouble.toFloat
        inline def toDouble: Double = r.n.toDouble / r.d.toDouble

end rational
