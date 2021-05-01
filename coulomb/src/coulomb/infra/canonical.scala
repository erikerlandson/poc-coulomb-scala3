package coulomb.infra

import scala.util.NotGiven

import coulomb.rational.Rational
import coulomb.{ %*, %/, %^ }
import coulomb.define.*

trait CanonicalSig[U]:
    type Res
    val coef: Rational

object CanonicalSig:
    // I can reuse this instead of creating new copies each time
    final lazy val sig1: CanonicalSig[1] { type Res = SNil } = new CanonicalSig[1] {
        type Res = SNil
        val coef = Rational.const1
    }

    transparent inline given CanonicalSig[1] = sig1

    // cache signature objects on the BaseUnit itself so we can reuse them
    transparent inline given [U](using bu: BaseUnit[U]): CanonicalSig[U] = bu.sig

    transparent inline given [U, D](using
        du: DerivedUnit[U, D], ds: CanonicalSig[D]):
            CanonicalSig[U] =
        new CanonicalSig[U]:
            type Res = ds.Res
            val coef = du.coef * ds.coef

    transparent inline given [L, R](using
        sl: CanonicalSig[L], sr: CanonicalSig[R], rs: UnifySigMul[sl.Res, sr.Res]):
            CanonicalSig[L %* R] =
        new CanonicalSig[L %* R]:
            type Res = rs.Res
            val coef = sl.coef * sr.coef

    transparent inline given [L, R](using
        sl: CanonicalSig[L], sr: CanonicalSig[R], rs: UnifySigDiv[sl.Res, sr.Res]):
            CanonicalSig[L %/ R] =
        new CanonicalSig[L %/ R]:
            type Res = rs.Res
            val coef = sl.coef / sr.coef

    transparent inline given [U, E](using
        su: CanonicalSig[U], e: RatVal[E], rs: UnifySigPow[E, su.Res]):
            CanonicalSig[U %^ E] =
        new CanonicalSig[U %^ E]:
            type Res = rs.Res
            val coef = su.coef.pow(e.value)

    transparent inline given [T](using bu: ImpliedBU[T]): CanonicalSig[T] =
        new CanonicalSig[T]:
            type Res = (T, 1) %: SNil
            val coef = Rational.const1

trait ImpliedBU[T]
object ImpliedBU:
    given [T](using
        NotGiven[coulomb.policy.StrictUnitExpressions],
        NotGiven[StrictUnitExpr[T]]):
            ImpliedBU[T] =
        new ImpliedBU[T] {}

trait StrictUnitExpr[T]
object StrictUnitExpr:
    given s0: StrictUnitExpr[1] with {}
    given s1[U](using bu: BaseUnit[U]): StrictUnitExpr[U] = bu.strict
    given s2[U](using du: DerivedUnit[U, _]): StrictUnitExpr[U] = du.strict
    given s3[L, R]: StrictUnitExpr[L %* R] = new StrictUnitExpr[L %* R] {}
    given s4[L, R]: StrictUnitExpr[L %/ R] = new StrictUnitExpr[L %/ R] {}
    given s5[L, R]: StrictUnitExpr[L %^ R] = new StrictUnitExpr[L %^ R] {}
