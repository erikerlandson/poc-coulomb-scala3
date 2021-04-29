package coulomb.infra

import scala.util.NotGiven
import scala.annotation.implicitNotFound

import coulomb.rational.*

trait SNil
trait %:[Head, Tail]

@implicitNotFound("Could not prove ${T1} was not equal to ${T2}")
trait =/=[T1, T2]
object =/= :
    given [T1, T2](using NotGiven[T1 =:= T2]): =/=[T1, T2] =
        new =/=[T1, T2] {}

trait UnifySigMul[Sig1, Sig2]:
    type Res
object UnifySigMul:
    transparent inline given [Sig1, Sig2]: UnifySigMul[Sig1, Sig2] = ${ sigopsMeta.unifyMulMeta[Sig1, Sig2] }

trait UnifySigDiv[Sig1, Sig2]:
    type Res
object UnifySigDiv:
    transparent inline given [Sig1, Sig2]: UnifySigDiv[Sig1, Sig2] = ${ sigopsMeta.unifyDivMeta[Sig1, Sig2] }

trait UnifySigPow[Power, Sig]:
    type Res
object UnifySigPow:
    transparent inline given [Power, Sig]: UnifySigPow[Power, Sig] = ${ sigopsMeta.unifyPowMeta[Power, Sig] }

object sigopsMeta:
    import scala.quoted.*
    import scala.language.implicitConversions

    def unifyMulMeta[Sig1, Sig2](using Quotes, Type[Sig1], Type[Sig2]): Expr[UnifySigMul[Sig1, Sig2]] =
        import quotes.reflect.*
        unifyOp(TypeRepr.of[Sig1], TypeRepr.of[Sig2], _ + _).asType match
            case '[resT] => '{ new UnifySigMul[Sig1, Sig2] { type Res = resT } }

    def unifyDivMeta[Sig1, Sig2](using Quotes, Type[Sig1], Type[Sig2]): Expr[UnifySigDiv[Sig1, Sig2]] =
        import quotes.reflect.*
        unifyOp(TypeRepr.of[Sig1], TypeRepr.of[Sig2], _ - _).asType match
            case '[resT] => '{ new UnifySigDiv[Sig1, Sig2] { type Res = resT } }

    def unifyPowMeta[Power, Sig](using Quotes, Type[Power], Type[Sig]): Expr[UnifySigPow[Power, Sig]] =
        import quotes.reflect.*
        unifyPow(TypeRepr.of[Power], TypeRepr.of[Sig]).asType match
            case '[resT] => '{ new UnifySigPow[Power, Sig] { type Res = resT } }

    def unifyOp(using Quotes)(
            sig1: quotes.reflect.TypeRepr, sig2: quotes.reflect.TypeRepr,
            op: (Rational, Rational) => Rational): quotes.reflect.TypeRepr =
        import quotes.reflect.*
        sig2 match
            case signil() => sig1
            case sigcons(u, e, tail) => unifyOp(insertTerm(u, e, sig1, op), tail, op)
            case _ => { report.error(s"Unsupported type ${sig2.show}"); TypeRepr.of[Nothing] }

    def insertTerm(using Quotes)(
            u: quotes.reflect.TypeRepr, e: Rational,
            sig: quotes.reflect.TypeRepr,
            op: (Rational, Rational) => Rational): quotes.reflect.TypeRepr =
        import quotes.reflect.*
        sig match
            case signil() => sigcons(u, e, signil())
            case sigcons(u0, e0, tail) if (u =:= u0) => 
                val ei = op(e0, e)
                if (ei === 0) tail else sigcons(u, ei, tail)
            case sigcons(u0, e0, tail) => sigcons(u0, e0, insertTerm(u, e, tail, op))
            case _ => { report.error(s"Unsupported type ${sig.show}"); TypeRepr.of[Nothing] }

    def unifyPow(using Quotes)(power: quotes.reflect.TypeRepr, sig: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
        val ratexp(e) = power
        if (e === 0) signil() else unifyPowTerm(e, sig)

    def unifyPowTerm(using Quotes)(e: Rational, sig: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
        import quotes.reflect.*
        sig match
            case signil() => signil()
            case sigcons(u, e0, tail) => sigcons(u, e0 * e, unifyPowTerm(e, tail))
            case _ => { report.error(s"Unsupported type ${sig.show}"); TypeRepr.of[Nothing] }

    object signil:
        def apply(using Quotes)(): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[SNil]

        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Boolean =
            tr =:= quotes.reflect.TypeRepr.of[SNil]

    object sigcons:
        def apply(using Quotes)(u: quotes.reflect.TypeRepr, e: Rational, tail: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
            sctc().appliedTo(List(t2tc().appliedTo(List(u, ratexp(e))), tail))

        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Option[(quotes.reflect.TypeRepr, Rational, quotes.reflect.TypeRepr)] =
            import quotes.reflect.*
            tr match
                case AppliedType(sctc(), List(AppliedType(t2tc(), List(u, ratexp(e))), tail)) =>
                    Some((u, e, tail))
                case _ => None

    object sctc:
        def apply(using Quotes)(): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[%:]

        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Boolean =
            tr =:= quotes.reflect.TypeRepr.of[%:]

    object t2tc:
        def apply(using Quotes)(): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Tuple2]

        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Boolean =
            tr =:= quotes.reflect.TypeRepr.of[Tuple2]

    object ratexp:
        def apply(using Quotes)(e: Rational): quotes.reflect.TypeRepr =
            import quotes.reflect.*
            if (e.d == 1) then
                ConstantType(IntConstant(e.n.toInt))
            else
                TypeRepr.of[/%].appliedTo(List(ConstantType(IntConstant(e.n.toInt)), ConstantType(IntConstant(e.d.toInt))))

        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Option[Rational] =
            import quotes.reflect.*
            tr match
                case ratlt(n, d) => Some(Rational(n, d))
                case intlt(n) => Some(Rational(n, 1))
                case _ => None

    object ratlt:
        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Option[(Int, Int)] =
             import quotes.reflect.*
             tr match
                case AppliedType(tc, List(ConstantType(IntConstant(n)), ConstantType(IntConstant(d)))) if (tc =:= TypeRepr.of[/%]) =>
                    Some((n, d))
                case _ => None

    object intlt:
        def unapply(using Quotes)(tr: quotes.reflect.TypeRepr): Option[Int] =
             import quotes.reflect.*
             tr match
                case ConstantType(IntConstant(i)) => Some(i)
                case _ => None
