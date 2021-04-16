package coulomb.rational

class Rational private (val n: BigInt, val d: BigInt) extends Serializable:
    import Rational.canonical

    override def toString: String =
        if (d == 1) s"$n" else s"$n/$d"

    inline def +(rhs: Rational): Rational =
        canonical((n * rhs.d) + (rhs.n * d), d * rhs.d)

    inline def -(rhs: Rational): Rational =
        canonical((n * rhs.d) - (rhs.n * d), d * rhs.d)

    inline def *(rhs: Rational): Rational =
        canonical(n * rhs.n, d * rhs.d)

    inline def /(rhs: Rational): Rational =
        canonical(n * rhs.d, d * rhs.n)

    inline def unary_- : Rational =
        canonical(-n, d)

    def pow(e: Int): Rational =
        if (e < 0) then
            canonical(d.pow(-e), n.pow(-e))
        else if (e == 0) then
            require(n != 0)
            canonical(1, 1)
        else if (e == 1) then
            this
        else
            canonical(n.pow(e), d.pow(e))

    def root(e: Int): Rational =
        import scala.math
        require(e != 0)
        if (e < 0) then
            canonical(d, n).root(-e)
        else if (e == 1) then
            this
        else if (n < 0) then
            require(e % 2 == 1)
            -((-this).root(e))
        else
            val nr = math.pow(n.toDouble, 1.0 / e.toDouble)
            val dr = math.pow(d.toDouble, 1.0 / e.toDouble)
            if ((nr == math.rint(nr)) && (dr == math.rint(dr))) then
                canonical(nr.toLong, dr.toLong)
            else
                Rational(nr / dr)

    inline def pow(e: Rational): Rational = this.pow(e.n.toInt).root(e.d.toInt)

    inline def toInt: Int = toDouble.toInt
    inline def toLong: Long = toDouble.toLong
    inline def toFloat: Float = toDouble.toFloat
    inline def toDouble: Double = n.toDouble / d.toDouble

    inline def === (rhs: Rational): Boolean = (n == rhs.n) && (d == rhs.d)
    inline def =/= (rhs: Rational): Boolean = !(this === rhs)

    inline def < (rhs: Rational): Boolean = (n * rhs.d) < (rhs.n * d)
    inline def > (rhs: Rational): Boolean = rhs < this
    inline def <= (rhs: Rational): Boolean = !(this > rhs)
    inline def >= (rhs: Rational): Boolean = !(this < rhs)
end Rational

object Rational:
    import scala.math.*

    inline def apply(n: BigInt, d: BigInt): Rational = canonical(n, d)

    inline def apply(r: Rational): Rational = canonical(r.n, r.d)

    inline def apply(v: Int): Rational = canonical(v, 1)
    inline def apply(v: Long): Rational = canonical(v, 1)
    inline def apply(v: Float): Rational = apply(v.toDouble)

    def apply(v: Double): Rational =
        if (abs(v) == 0.0) then canonical(0, 1)
        else
            // IEEE double precision guaranteed 15 base-10 digits of precision
            val e = 15 - (floor(log10(abs(v))).toInt + 1)
            val (np10, dp10) = if (e < 0) then (-e, 0) else (0, e)
            val vi = v * scala.math.pow(10, e)
            val n = BigInt(vi.toLong) * BigInt(10).pow(np10)
            val d = BigInt(10).pow(dp10)
            canonical(n, d)

    // intended to be the single safe way to construct a canonical rational
    // every construction of a new Rational should reduce to some call to this method
    private [rational] def canonical(n: BigInt, d: BigInt): Rational =
        require(d != 0, "Rational denominator cannot be zero")
        if (n == 0)
            // canonical zero is 0/1
            new Rational(0, 1)
        else if (d < 0) then
            // canonical denominator is always positive
            canonical(-n, -d)
        else
            // canonical rationals are fully reduced
            val g = n.gcd(d)
            new Rational(n / g, d / g)

    given Conversion[Int, Rational] with
        def apply(v: Int): Rational = Rational(v)
    given Conversion[Long, Rational] with
        def apply(v: Long): Rational = Rational(v)
    given Conversion[Float, Rational] with
        def apply(v: Float): Rational = Rational(v)
    given Conversion[Double, Rational] with
        def apply(v: Double): Rational = Rational(v)
end Rational
