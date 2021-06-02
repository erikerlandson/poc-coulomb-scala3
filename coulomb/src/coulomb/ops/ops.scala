package coulomb.ops

import scala.annotation.implicitNotFound

import coulomb.*

@implicitNotFound("Addition not defined in scope for Quantity[${VL}, ${UL}] and Quantity[${VR}, ${UR}]")
abstract class Add[VL, UL, VR, UR]:
    type VO
    type UO
    def apply(vl: VL, vr: VR): VO
