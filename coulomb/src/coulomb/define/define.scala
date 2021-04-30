package coulomb.define

import scala.language.implicitConversions

import coulomb.rational.Rational

/** Methods and values common to all unit and temperature definitions */
trait UnitDefinition:
    /** the full name of a unit, e.g. "meter" */
    val name: String
    /** the abbreviation of a unit, e.g. "m" for "meter" */
    val abbv: String

abstract class BaseUnit[U] extends UnitDefinition:
    import coulomb.infra.*
    // I can cache the signature with the definition and only compute it once
    final lazy val sig: CanonicalSig[U] { type Res = (U, 1) %: SNil } = new CanonicalSig[U] {
        type Res = (U, 1) %: SNil
        val coef = Rational.const1
    }

    override def toString = s"BaseUnit($name, $abbv)"

abstract class DerivedUnit[U, D] extends UnitDefinition:
    val coef: Rational
    override def toString = s"DerivedUnit($coef, $name, $abbv)"

abstract class PrefixUnit[U] extends DerivedUnit[U, 1]
