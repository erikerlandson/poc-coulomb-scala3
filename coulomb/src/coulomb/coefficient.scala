package coulomb

import scala.annotation.implicitNotFound

import coulomb.rational.Rational

@implicitNotFound("Units are not commensurate: no coefficient of conversion exists for unit types (${U1}) and (${U2})")
trait Coefficient[U1, U2]:
    val coef: Rational
    override def toString = s"Coefficient($coef)"

object Coefficient:
    inline given [U1, U2]: Coefficient[U1, U2] = ${ coulomb.infra.meta.coefficient[U1, U2] }
