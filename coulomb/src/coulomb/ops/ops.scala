package coulomb.ops

import scala.annotation.implicitNotFound

import coulomb.*
import coulomb.rational.Rational

@implicitNotFound("Addition not defined in scope for Quantity[${VL}, ${UL}] and Quantity[${VR}, ${UR}]")
abstract class Add[VL, UL, VR, UR]:
    type VO
    type UO
    def apply(vl: VL, vr: VR): VO

object standard:
    transparent inline given ua01[U]: Add[Double, U, Double, U] =
        new Add[Double, U, Double, U]:
            type VO = Double
            type UO = U
            def apply(vl: Double, vr: Double): Double = vl + vr

    transparent inline given ua01[UL, UR](using conv: Coefficient[UR, UL]): Add[Double, UL, Double, UR] =
        new Add[Double, UL, Double, UR]:
            type VO = Double
            type UO = UL
            val c = conv.coef.toDouble
            def apply(vl: Double, vr: Double): Double = vl + (c * vr)

    transparent inline given ua02[UL, UR](using conv: Coefficient[UR, UL]): Add[Int, UL, Double, UR] =
        new Add[Int, UL, Double, UR]:
            type VO = Double
            type UO = UL
            val c = conv.coef.toDouble
            def apply(vl: Int, vr: Double): Double = vl + (c * vr)
