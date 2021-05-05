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
    final lazy val canonical: CanonicalSig[U] { type Res = (U, 1) %: SNil } = new CanonicalSig[U] {
        type Res = (U, 1) %: SNil
        val coef = Rational.const1
    }

/*
    final lazy val standard: StandardSig[U] { type Res = (U, 1) %: SNil } = new StandardSig[U]:
        type Res = (U, 1) %: SNil
    final lazy val strict: StrictUnitExpr[U] = new StrictUnitExpr[U] {}
*/

    override def toString = s"BaseUnit($name, $abbv)"

abstract class DerivedUnit[U, D] extends UnitDefinition:
    import coulomb.infra.*
    val coef: Rational

/*
    final lazy val standard: StandardSig[U] { type Res = (U, 1) %: SNil } = new StandardSig[U]:
        type Res = (U, 1) %: SNil
    final lazy val strict: StrictUnitExpr[U] = new StrictUnitExpr[U] {}
*/

    override def toString = s"DerivedUnit($coef, $name, $abbv)"

// Not necessary, but allows meta-programming to be smarter with coefficients
abstract class DerivedUnit1[U, D] extends DerivedUnit[U, D]:
    val coef: Rational = Rational.const1

// prefix units are derived units of 1 ('unitless')
abstract class PrefixUnit[U] extends DerivedUnit[U, 1]
