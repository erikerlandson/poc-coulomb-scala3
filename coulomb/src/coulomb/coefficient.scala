package coulomb

import coulomb.rational.Rational

trait Coefficient[U1, U2]:
    val coef: Rational
object Coefficient:
    import scala.quoted.*

    inline given [U1, U2]: Coefficient[U1, U2] = $ { coefMeta[U1, U2] }

    def coefMeta[U1, U2](using Type[U1], Type[U2], Quotes): Expr[Coefficient[U1, U2]] =
        '{ new Coefficient[U1, U2] { val coef = Rational(1) } }
