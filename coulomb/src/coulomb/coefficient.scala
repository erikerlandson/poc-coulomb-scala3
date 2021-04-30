package coulomb

import coulomb.rational.Rational
import coulomb.infra.{ CanonicalSig, SNil }

trait Coefficient[U1, U2]:
    val coef: Rational
object Coefficient:
    inline given [U]: Coefficient[U, U] =
        new Coefficient[U, U] { val coef = Rational.const1 }

    inline given [U1, U2](using
        sig: CanonicalSig[U1 %/ U2],
        sig1: sig.Res =:= SNil):
            Coefficient[U1, U2] =
        new Coefficient[U1, U2] { val coef = sig.coef }
