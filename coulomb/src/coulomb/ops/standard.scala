package coulomb.ops.standard

import coulomb.*
import coulomb.ops.*

transparent inline given g1[U]: Add[Double, U, Double, U] =
    new Add[Double, U, Double, U]:
        type VO = Double
        type UO = U
        def apply(vl: Double, vr: Double): Double = vl + vr

transparent inline given g2[UL, UR](using coef: Coefficient[UR, UL]): Add[Double, UL, Double, UR] =
    new Add[Double, UL, Double, UR]:
        type VO = Double
        type UO = UL
        val c = coef.value.toDouble
        def apply(vl: Double, vr: Double): Double = vl + (c * vr)

transparent inline given g3[UL, UR](using coef: Coefficient[UR, UL]): Add[Int, UL, Double, UR] =
    new Add[Int, UL, Double, UR]:
        type VO = Double
        type UO = UL
        val c = coef.value.toDouble
        def apply(vl: Int, vr: Double): Double = vl + (c * vr)
