package coulomb.conversion.standard

import coulomb.*

given g1[U1, U2](using conv: Coefficient[U1, U2]):
        scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]] =
    new scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]]:
        val c = conv.coef.toDouble
        inline def apply(q: Quantity[Double, U1]): Quantity[Double, U2] =
            (q.value * c).withUnit[U2]
