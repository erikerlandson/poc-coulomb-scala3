package coulomb.infra

import scala.quoted.*

import coulomb.rational.Rational
import coulomb.rational.{ /%, /%+, /%-, /%* }
import coulomb.{ %*, %/, %^ }
import coulomb.define.*

trait CanonicalSig[U]:
    type Res
    val coef: Rational

object CanonicalSig:
    transparent inline given CanonicalSig[1] =
        new CanonicalSig[1]:
            type Res = SNil
            val coef = Rational(1)

    transparent inline given [U](using BaseUnit[U]): CanonicalSig[U] =
        new CanonicalSig[U]:
            type Res = (U, 1) %: SNil
            val coef = Rational(1)

    transparent inline given [U, D](using
            du: DerivedUnit[U, D],
            ds: CanonicalSig[D]): CanonicalSig[U] =
        new CanonicalSig[U]:
            type Res = ds.Res
            val coef = du.coef * ds.coef
