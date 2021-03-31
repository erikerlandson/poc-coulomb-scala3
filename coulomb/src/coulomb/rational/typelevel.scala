package coulomb.rational

import scala.annotation.implicitNotFound

// rational exponents at the type level
trait /%[L, R]

@implicitNotFound("${L} /%+ ${R} has no defined context")
trait /%+[L, R]:
    type Res
object /%+ :
    transparent inline given [L, R]: /%+[L, R] = ${ macros.addImpl[L, R] }

trait /%-[L, R]:
    type Res
object /%- :
    transparent inline given [L, R]: /%-[L, R] = ${ macros.subImpl[L, R] }

trait /%*[L, R]:
    type Res
object /%* :
    transparent inline given [L, R]: /%*[L, R] = ${ macros.mulImpl[L, R] }

trait /%/[L, R]:
    type Res
object /%/ :
    transparent inline given [L, R]: /%/[L, R] = ${ macros.divImpl[L, R] }

object macros:
    import scala.quoted.*

    def addImpl[L, R](using Type[L], Type[R], Quotes): Expr[/%+[L, R]] =
        resType(resVal[L, R](_ + _)) match
            case '[resT] => '{ new _root_.coulomb.rational./%+[L, R] { type Res = resT } }

    def subImpl[L, R](using Type[L], Type[R], Quotes): Expr[/%-[L, R]] =
        resType(resVal[L, R](_ - _)) match
            case '[resT] => '{ new _root_.coulomb.rational./%-[L, R] { type Res = resT } }

    def mulImpl[L, R](using Type[L], Type[R], Quotes): Expr[/%*[L, R]] =
        resType(resVal[L, R](_ * _)) match
            case '[resT] => '{ new _root_.coulomb.rational./%*[L, R] { type Res = resT } }

    def divImpl[L, R](using Type[L], Type[R], Quotes): Expr[/%/[L, R]] =
        resType(resVal[L, R](_ / _)) match
            case '[resT] => '{ new _root_.coulomb.rational./%/[L, R] { type Res = resT } }

    def resVal[L, R](op: (Rational, Rational) => Rational)(using Type[L], Type[R], Quotes): Rational =
        import quotes.reflect.*
        (TypeRepr.of[L], TypeRepr.of[R]) match
            case (IntLiteralType(lv), IntLiteralType(rv)) => op(Rational(lv, 1), Rational(rv, 1))
            case (RationalType(ln, ld), RationalType(rn, rd)) => op(Rational(ln, ld), Rational(rn, rd))
            case (IntLiteralType(lv), RationalType(rn, rd)) => op(Rational(lv, 1), Rational(rn, rd))
            case (RationalType(ln, ld), IntLiteralType(rv)) => op(Rational(ln, ld), Rational(rv, 1))
            case _ => { report.error("Unsupported types for Rational function"); Rational(0, 1) }

    def resType(res: Rational)(using Quotes): Type[?] =
        import quotes.reflect.*
        val tres: TypeRepr = if (res.d == 1) then
            ConstantType(IntConstant(res.n.toInt))
        else
            val resTC = TypeRepr.of[_root_.coulomb.rational./%]
            resTC.appliedTo(List(ConstantType(IntConstant(res.n.toInt)), ConstantType(IntConstant(res.d.toInt))))
        tres.asType

    object IntLiteralType:
        def unapply(tr: Any)(using Quotes): Option[Int] =
             import quotes.reflect.*
             try
                 // I haven't figured out a way to specify TypeRepr for parameter 'tr'.
                 // Wrapping this in try/catch adequately fakes type soundness here, since if it fails,
                 // the correct answer is None anyway
                 tr.asInstanceOf[TypeRepr] match
                    case ConstantType(IntConstant(i)) => Some(i)
                    case _ => None
             catch _ => None

    object RationalType:
        def unapply(tr: Any)(using Quotes): Option[(Int, Int)] =
             import quotes.reflect.*
             try
                 tr.asInstanceOf[TypeRepr] match
                    case AppliedType(tc, List(ConstantType(IntConstant(n)), ConstantType(IntConstant(d)))) if (tc.typeSymbol.name == "/%") =>
                        Some((n, d))
                    case _ => None
             catch _ => None

end macros
