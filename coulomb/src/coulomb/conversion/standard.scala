package coulomb.conversion.standard

import scala.Conversion

import coulomb.*

transparent inline given convStandard1U[VF, VT, U]: Conversion[Quantity[VF, U], Quantity[VT, U]] =
    ${ meta.convStandard1U[VF, VT, U] }

transparent inline given convStandard2U[VF, UF, VT, UT]: Conversion[Quantity[VF, UF], Quantity[VT, UT]] =
    ${ meta.convStandard2U[VF, UF, VT, UT] }

object meta:
    import scala.quoted.*
    import coulomb.infra.meta.*

    def convStandard1U[VF :Type, VT :Type, U :Type](using Quotes):
            Expr[Conversion[Quantity[VF, U], Quantity[VT, U]]] =
        import quotes.reflect.*
        (TypeRepr.of[VF], TypeRepr.of[VT]) match
            case (typeDouble(), typeDouble()) =>
                '{ new Conversion[Quantity[VF, U], Quantity[VT, U]] { def apply(q: Quantity[Double, U]): Quantity[Double, U] = q } }
            case (typeFloat(), typeDouble()) =>
                '{ new Conversion[Quantity[VF, U], Quantity[VT, U]] { def apply(q: Quantity[Float, U]): Quantity[Double, U] = q.value.toDouble.withUnit[U] } }
            case (typeInt(), typeDouble()) =>
                '{ new Conversion[Quantity[VF, U], Quantity[VT, U]] { def apply(q: Quantity[Int, U]): Quantity[Double, U] = q.value.toDouble.withUnit[U] } }
            case (typeLong(), typeDouble()) =>
                '{ new Conversion[Quantity[VF, U], Quantity[VT, U]] { def apply(q: Quantity[Long, U]): Quantity[Double, U] = q.value.toDouble.withUnit[U] } }
            case _ =>
                report.error(s"undefined Conversion pattern in convStandard1U")
                '{ new Conversion[Quantity[VF, U], Quantity[VT, U]] { def apply(q: Quantity[VF, U]): Quantity[VT, U] = ??? } }

    def convStandard2U[VF :Type, UF :Type, VT :Type, UT :Type](using Quotes):
            Expr[Conversion[Quantity[VF, UF], Quantity[VT, UT]]] =
        import quotes.reflect.*
        val (uF, uT) = (TypeRepr.of[UF], TypeRepr.of[UT])
        // If the case of identical units was not handled by convStandard1U, something has gone wrong
        require(!(uF =:= uT))
        val cf = coef(uF, uT)
        (TypeRepr.of[VF], TypeRepr.of[VT]) match
            case (typeDouble(), typeDouble()) => '{
                new Conversion[Quantity[VF, UF], Quantity[VT, UT]]:
                    val c = ${cf}.toDouble
                    def apply(q: Quantity[Double, UF]): Quantity[Double, UT] = (q.value * c).withUnit[UT]
            }
            case _ =>
                report.error(s"undefined Conversion pattern in convStandard2U")
                '{ new Conversion[Quantity[VF, UF], Quantity[VT, UT]] { def apply(q: Quantity[VF, UF]): Quantity[VT, UT] = ??? } }

/*
given g1[U]: scala.Conversion[Quantity[Double, U], Quantity[Double, U]] =
    new scala.Conversion[Quantity[Double, U], Quantity[Double, U]]:
        inline def apply(q: Quantity[Double, U]): Quantity[Double, U] = q

given g2[U1, U2](using coef: Coefficient[U1, U2]):
        scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]] =
    new scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]]:
        val c = coef.value.toDouble
        inline def apply(q: Quantity[Double, U1]): Quantity[Double, U2] =
            (q.value * c).withUnit[U2]
*/
