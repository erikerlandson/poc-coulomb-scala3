package coulomb.define

import coulomb.infra.UnitTypeName
import coulomb.rational.Rational

/** Methods and values common to all unit and temperature definitions */
trait UnitDefinition:
    /** the full name of a unit, e.g. "meter" */
    def name: String
    /** the abbreviation of a unit, e.g. "m" for "meter" */
    def abbv: String

class BaseUnit[U](val name: String, val abbv: String) extends UnitDefinition:
    override def toString = s"BaseUnit($name, $abbv)"
object BaseUnit:
    def apply[U](name: String = "", abbv: String = "")(using ut: UnitTypeName[U]): BaseUnit[U] =
        val n = if (name != "") name else ut.name.toLowerCase()
        val a = if (abbv != "") abbv else n.take(1)
        new BaseUnit[U](n, a)

class DerivedUnit[U, D](val coef: Rational, val name: String, val abbv: String) extends UnitDefinition:
    override def toString = s"DerivedUnit($coef, $name, $abbv)"
object DerivedUnit:
    def apply[U, D](coef: Rational = Rational(1), name: String = "", abbv: String = "")(using
            ut: UnitTypeName[U]): DerivedUnit[U, D] =
        require(coef > Rational(0), "Unit coefficients must be strictly > 0")
        val n = if (name != "") name else ut.name.toLowerCase()
        val a = if (abbv != "") abbv else n.take(1)
        new DerivedUnit[U, D](coef, n, a)

/** methods, constructors and other static definitions for defining prefix units */
object PrefixUnit:
    def apply[U](coef: Rational = Rational(1), name: String = "", abbv: String = "")(implicit
            ut: UnitTypeName[U]): DerivedUnit[U, 1] =
        DerivedUnit[U, 1](coef, name, abbv)
