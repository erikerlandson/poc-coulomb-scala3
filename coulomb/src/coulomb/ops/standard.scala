package coulomb.ops.standard

import coulomb.*
import coulomb.ops.*

transparent inline given addStandard[VL, UL, VR, UR]: Add[VL, UL, VR, UR] =
    ${ meta.addStandard[VL, UL, VR, UR] }

object meta:
    import scala.quoted.*
    import coulomb.infra.meta.*

    def addStandard[VL :Type, UL :Type, VR :Type, UR :Type](using Quotes): Expr[Add[VL, UL, VR, UR]] =
        import quotes.reflect.*
        val (uL, uR) = (TypeRepr.of[UL], TypeRepr.of[UR])
        if (uL =:= uR) then
            // units are the same, so no coefficient is necessary
            (TypeRepr.of[VL], TypeRepr.of[VR]) match
                case (typeDouble(), typeDouble()) => '{
                    new Add[VL, UL, VR, UR]:
                        type VO = Double
                        type UO = UL
                        def apply(vl: Double, vr: Double): Double = vl + vr
                }
                case _ =>
                    report.error(s"addition not defined for these types")
                    '{ new Add[VL, UL, VR, UR] { type VO = Int; type UO = Nothing; def apply(vl: VL, vr: VR): VO = 0 } }
        else
            // units are not identical: get coefficient (or fail)
            val cf = coef(uR, uL) // get coefficient from right to left
            (TypeRepr.of[VL], TypeRepr.of[VR]) match
                case (typeDouble(), typeDouble()) => '{
                    new Add[VL, UL, VR, UR]:
                        type VO = Double
                        type UO = UL
                        val c = ${cf}.toDouble
                        def apply(vl: Double, vr: Double): Double = vl + (c * vr)
                }
                case _ =>
                    report.error(s"addition not defined for these types")
                    '{ new Add[VL, UL, VR, UR] { type VO = Int; type UO = Nothing; def apply(vl: VL, vr: VR): VO = 0 } }

/*
transparent inline given addxUCxDD[U]: Add[Double, U, Double, U] =
    new Add[Double, U, Double, U]:
        type VO = Double
        type UO = U
        def apply(vl: Double, vr: Double): Double = vl + vr

transparent inline given addxUCxFD[U]: Add[Float, U, Double, U] =
    new Add[Float, U, Double, U]:
        type VO = Double
        type UO = U
        def apply(vl: Float, vr: Double): Double = vl + vr

transparent inline given addxUCxID[U]: Add[Int, U, Double, U] =
    new Add[Int, U, Double, U]:
        type VO = Double
        type UO = U
        def apply(vl: Int, vr: Double): Double = vl + vr

transparent inline given addxUCxLD[U]: Add[Long, U, Double, U] =
    new Add[Long, U, Double, U]:
        type VO = Double
        type UO = U
        def apply(vl: Long, vr: Double): Double = vl + vr

transparent inline given addxUCxDF[U]: Add[Double, U, Float, U] =
    new Add[Double, U, Float, U]:
        type VO = Double
        type UO = U
        def apply(vl: Double, vr: Float): Double = vl + vr

transparent inline given addxUCxFF[U]: Add[Float, U, Float, U] =
    new Add[Float, U, Float, U]:
        type VO = Float
        type UO = U
        def apply(vl: Float, vr: Float): Float = vl + vr

transparent inline given addxUCxIF[U]: Add[Int, U, Float, U] =
    new Add[Int, U, Float, U]:
        type VO = Float
        type UO = U
        def apply(vl: Int, vr: Float): Float = vl + vr

transparent inline given addxUCxLF[U]: Add[Long, U, Float, U] =
    new Add[Long, U, Float, U]:
        type VO = Float
        type UO = U
        def apply(vl: Long, vr: Float): Float = vl + vr

transparent inline given addxUCxDI[U]: Add[Double, U, Int, U] =
    new Add[Double, U, Int, U]:
        type VO = Double
        type UO = U
        def apply(vl: Double, vr: Int): Double = vl + vr

transparent inline given addxUCxFI[U]: Add[Float, U, Int, U] =
    new Add[Float, U, Int, U]:
        type VO = Float
        type UO = U
        def apply(vl: Float, vr: Int): Float = vl + vr

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
*/
