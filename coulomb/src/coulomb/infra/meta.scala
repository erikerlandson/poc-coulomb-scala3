package coulomb.infra

import coulomb.rational.{ Rational, /% }
import coulomb.{ %*, %/, %^ }

import coulomb.define.*

object meta:
    import scala.quoted.*
    import scala.language.implicitConversions

    def canonical[U](using Quotes, Type[U]): Expr[CanonicalSig[U]] =
        import quotes.reflect.*
        // only the top-level CanonicalSig expression is actually instantiated. This is
        // more efficient than the pure chained-given implementation, which must also
        // instantiate all the intermediate forms.
        val (cs, _, _) = sigrec(TypeRepr.of[U])
        cs.asInstanceOf[Expr[CanonicalSig[U]]]

    // returns tuple: (expr-for-canonical, expr-for-coef, type-of-Res)
    def sigrec(using Quotes)(u: quotes.reflect.TypeRepr):
            (Expr[CanonicalSig[?]], Expr[Rational], quotes.reflect.TypeRepr) =
        import quotes.reflect.*
        u match
            case unitless(can) => (can, '{ Rational.const1 }, signil())
            case baseunit(can) => (can, '{ Rational.const1 }, sigcons(u, Rational.const1, signil()))
            case derivedunit(can, coef, sig) => (can, coef, sig)
            case AppliedType(op, List(lu, ru)) if (op =:= TypeRepr.of[%*]) =>
                val (_, lcoef, lsig) = sigrec(lu)
                val (_, rcoef, rsig) = sigrec(ru)
                val ucoef = if (coefIs1(lcoef)) rcoef else if (coefIs1(rcoef)) lcoef else '{ $lcoef * $rcoef }
                val usig = unifyOp(lsig, rsig, _ + _)
                val ucan = (u.asType, usig.asType) match
                    case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $ucoef } }
                (ucan, ucoef, usig)
            case AppliedType(op, List(lu, ru)) if (op =:= TypeRepr.of[%/]) =>
                val (_, lcoef, lsig) = sigrec(lu)
                val (_, rcoef, rsig) = sigrec(ru)
                val ucoef = if (coefIs1(rcoef)) lcoef else '{ $lcoef / $rcoef }
                val usig = unifyOp(lsig, rsig, _ - _)
                val ucan = (u.asType, usig.asType) match
                    case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $ucoef } }
                (ucan, ucoef, usig)
            case AppliedType(op, List(b, p)) if (op =:= TypeRepr.of[%^]) =>
                val (_, bcoef, bsig) = sigrec(b)
                val ratexp(e) = p
                if (e === Rational.const0)
                    val ucoef = '{ Rational.const1 }
                    val usig = signil()
                    val ucan = (u.asType, usig.asType) match
                        case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $ucoef } }
                    (ucan, ucoef, usig)
                else if (e === Rational.const1)
                    val ucan = (u.asType, bsig.asType) match
                        case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $bcoef } }
                    (ucan, bcoef, bsig)
                else
                    val ucoef = if (coefIs1(bcoef)) bcoef
                                else if (e.d == 1) '{ ${bcoef}.pow(${Expr(e.n.toInt)}) }
                                else '{ ${bcoef}.pow(${Expr(e.n.toInt)}).root(${Expr(e.d.toInt)}) }
                    val usig = unifyPow(p, bsig)
                    val ucan = (u.asType, usig.asType) match
                        case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $ucoef } }
                    (ucan, ucoef, usig)
            // we consider any other type for "promotion" to base-unit only if
            // it does not match the strict unit expression forms above, and
            // if the strict unit expression policy has not been enabled
            case _ if (!strictunitexprs) =>
                val ucoef = '{ Rational.const1 }
                val usig = sigcons(u, Rational.const1, signil())
                val ucan = (u.asType, usig.asType) match
                    case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $ucoef } }
                (ucan, ucoef, usig)
            case _ => { report.error(s"unknown unit expression in sigrec: $u"); csErr }

    def strictunitexprs(using Quotes): Boolean =
        import quotes.reflect.*
        Implicits.search(TypeRepr.of[coulomb.policy.StrictUnitExpressions]) match
                case _: ImplicitSearchSuccess => true
                case _ => false

    object unitless:
        def unapply(using Quotes)(u: quotes.reflect.TypeRepr): Option[Expr[CanonicalSig[1]]] =
            import quotes.reflect.*
            if (u =:= TypeRepr.of[1]) then Some('{ CanonicalSig.canonical1 }) else None

    object baseunit:
        def unapply(using Quotes)(u: quotes.reflect.TypeRepr): Option[Expr[CanonicalSig[?]]] =
            import quotes.reflect.*
            Implicits.search(TypeRepr.of[BaseUnit].appliedTo(u)) match
                case iss: ImplicitSearchSuccess =>
                    val bu = iss.tree.asExpr.asInstanceOf[Expr[BaseUnit[_]]]
                    Some('{ ${bu}.canonical })
                case _ => None

    object derivedunit:
        def unapply(using Quotes)(u: quotes.reflect.TypeRepr): Option[(Expr[CanonicalSig[?]], Expr[Rational], quotes.reflect.TypeRepr)] =
            import quotes.reflect.*
            Implicits.search(TypeRepr.of[DerivedUnit].appliedTo(List(u, TypeBounds.empty))) match
                case iss: ImplicitSearchSuccess =>
                    val AppliedType(_, List(_, d)) = iss.tree.tpe.baseType(TypeRepr.of[DerivedUnit].typeSymbol)
                    val (_, dcoef, dsig) = sigrec(d)
                    val du = iss.tree.asExpr.asInstanceOf[Expr[DerivedUnit[_, _]]]
                    // I currently have no way to test if ${du}.coef is equal to 1: it can be
                    // declared with any value and I have not been able to evaluate these expressions
                    // at meta/compile time.
                    val ucoef = if (coefIs1(dcoef)) '{ ${du}.coef } else '{ $dcoef * ${du}.coef }
                    val ucan = (u.asType, dsig.asType) match
                        case ('[uT], '[sT]) => '{ new CanonicalSig[uT] { type Res = sT; val coef = $ucoef } }
                    Some((ucan, ucoef, dsig))
                case _ => None

    // evaluating these at compilation time is not working, so the best I currently
    // know how to do is structural test for Rational.const1
    def coefIs1(using Quotes)(expr: Expr[Rational]): Boolean =
        expr.matches('{ Rational.const1 })

    def csErr(using Quotes):
            (Expr[CanonicalSig[?]], Expr[Rational], quotes.reflect.TypeRepr) =
        import quotes.reflect.*
        ('{ new CanonicalSig[Nothing] { type Res = Nothing; val coef = Rational.const0 } },
         '{ Rational.const0 },
         TypeRepr.of[Nothing])

    // keep this for reference
    def summonString[T](using Quotes, Type[T]): String =
        import quotes.reflect.*
        Expr.summon[T] match
            case None => "None"
            case Some(e) => s"${e.show}   ${e.asTerm.show(using Printer.TreeStructure)}"

/*
    def ratval[R](using Quotes, Type[R]): Expr[RatVal[R]] =
        import quotes.reflect.*
        val ratexp(r) = TypeRepr.of[R]
        '{ new RatVal[R] { val value = Rational(${Expr(r.n)}, ${Expr(r.d)}) } }
*/

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
            case signil() => sigcons(u, op(Rational(0), e), signil())
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
