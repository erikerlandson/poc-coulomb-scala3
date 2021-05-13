package coulomb

import coulomb.rational.Rational

trait Coefficient[U1, U2]:
    val coef: Rational
object Coefficient:
    transparent inline given [U1, U2]: Coefficient[U1, U2] = ${ coulomb.infra.meta.coefficient[U1, U2] }
