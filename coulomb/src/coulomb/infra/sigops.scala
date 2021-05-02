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
    transparent inline given [Sig1, Sig2]: UnifySigMul[Sig1, Sig2] = ${ meta.unifyMulMeta[Sig1, Sig2] }

trait UnifySigDiv[Sig1, Sig2]:
    type Res
object UnifySigDiv:
    transparent inline given [Sig1, Sig2]: UnifySigDiv[Sig1, Sig2] = ${ meta.unifyDivMeta[Sig1, Sig2] }

trait UnifySigPow[Power, Sig]:
    type Res
object UnifySigPow:
    transparent inline given [Power, Sig]: UnifySigPow[Power, Sig] = ${ meta.unifyPowMeta[Power, Sig] }
