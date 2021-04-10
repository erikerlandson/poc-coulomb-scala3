package coulomb.infra

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.any.*

import scala.util.NotGiven
import scala.annotation.implicitNotFound

import coulomb.rational.{ /%, /%+, /%-, /%* }

trait SNil
trait %:[Head, Tail]

@implicitNotFound("Could not prove ${T1} was not equal to ${T2}")
trait =/=[T1, T2]
object =/= :
    given [T1, T2](using NotGiven[T1 =:= T2]): =/=[T1, T2] =
        new =/=[T1, T2] {}

trait InsertSigMul[Unit, Power, Sig]:
    type Res
object InsertSigMul:
    type Eval[U, P, S, R] = InsertSigMul[U, P, S] { type Res = R }

    given [U, P]: Eval[U, P, SNil, (U, P) %: SNil] =
        new InsertSigMul[U, P, SNil] { type Res = (U, P) %: SNil }

    given [U, P, P0, PR, ST0](using /%+.Eval[P0, P, PR], PR =/= 0):
      Eval[U, P, (U, P0) %: ST0, (U, PR) %: ST0] =
        new InsertSigMul[U, P, (U, P0) %: ST0] { type Res = (U, PR) %: ST0 }

    given [U, P, P0, ST0](using /%+.Eval[P0, P, 0]):
      Eval[U, P, (U, P0) %: ST0, ST0] =
        new InsertSigMul[U, P, (U, P0) %: ST0] { type Res = ST0 }

    given [U, P, U0, P0, ST0, ST](using U =/= U0, Eval[U, P, ST0, ST]):
      Eval[U, P, (U0, P0) %: ST0, (U0, P0) %: ST] =
        new InsertSigMul[U, P, (U0, P0) %: ST0] { type Res = (U0, P0) %: ST }
