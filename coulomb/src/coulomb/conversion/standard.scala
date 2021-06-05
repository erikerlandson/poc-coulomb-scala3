package coulomb.conversion.standard

import coulomb.*

given g1[U]: scala.Conversion[Quantity[Double, U], Quantity[Double, U]] =
    new scala.Conversion[Quantity[Double, U], Quantity[Double, U]]:
        inline def apply(q: Quantity[Double, U]): Quantity[Double, U] = q

given g2[U1, U2](using coef: Coefficient[U1, U2]):
        scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]] =
    new scala.Conversion[Quantity[Double, U1], Quantity[Double, U2]]:
        val c = coef.value.toDouble
        inline def apply(q: Quantity[Double, U1]): Quantity[Double, U2] =
            (q.value * c).withUnit[U2]
