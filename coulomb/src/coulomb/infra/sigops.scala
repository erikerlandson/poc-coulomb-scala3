package coulomb.infra

import scala.compiletime.ops.int.*
import scala.compiletime.ops.boolean.*
import scala.compiletime.ops.any.*

import scala.util.NotGiven
import scala.annotation.implicitNotFound

import coulomb.rational.{ /%, /%+, /%-, /%* }

trait SNil
trait %:[Head, Tail]

trait UnifySigMul[Sig1, Sig2]:
    type Res
object UnifySigMul:
    transparent inline given [S]: UnifySigMul[S, SNil] =
        new UnifySigMul[S, SNil] { type Res = S }

    transparent inline given [S, U, P, ST](using
        s: InsertSigMul[U, P, S],
        re: UnifySigMul[s.Res, ST]):
            UnifySigMul[S, (U, P) %: ST] =
        new UnifySigMul[S, (U, P) %: ST] { type Res = re.Res }

trait UnifySigDiv[Sig1, Sig2]:
    type Res
object UnifySigDiv:
    transparent inline given [S]: UnifySigDiv[S, SNil] =
        new UnifySigDiv[S, SNil] { type Res = S }

    transparent inline given [S, U, P, ST](using
        np: 0 /%- P,
        s: InsertSigMul[U, np.Res, S],
        re: UnifySigDiv[s.Res, ST]):
            UnifySigDiv[S, (U, P) %: ST] =
        new UnifySigDiv[S, (U, P) %: ST] { type Res = re.Res }

trait UnifySigPow[Power, Sig]:
    type Res
object UnifySigPow:
    transparent inline given [S]: UnifySigPow[0, S] =
        new UnifySigPow[0, S] { type Res = SNil }

    transparent inline given [P](using P =/= 0): UnifySigPow[P, SNil] =
        new UnifySigPow[P, SNil] { type Res = SNil }

    transparent inline given [P, U0, P0, ST](using
        nz: P =/= 0,
        re: UnifySigPow[P, ST],
        p: P /%* P0):
            UnifySigPow[P, (U0, P0) %: ST] =
        new UnifySigPow[P, (U0, P0) %: ST] { type Res = (U0, p.Res) %: re.Res }

trait InsertSigMul[Unit, Power, Sig]:
    type Res
object InsertSigMul:
    // basis case: inserting to "empty" (unitless) signature
    transparent inline given [U, P]: InsertSigMul[U, P, SNil] =
        new InsertSigMul[U, P, SNil] { type Res = (U, P) %: SNil }

    // units match and exponents don't cancel: add their exponents
    transparent inline given [U, P, P0, ST0](using p: P0 /%+ P, pnz: p.Res =/= 0):
            InsertSigMul[U, P, (U, P0) %: ST0] =
        new InsertSigMul[U, P, (U, P0) %: ST0] { type Res = (U, p.Res) %: ST0 }

    // units match and exponents cancel out, so remove from the signature
    transparent inline given [U, P, P0, ST0](using p: P0 /%+ P, pz: p.Res =:= 0):
            InsertSigMul[U, P, (U, P0) %: ST0] =
        new InsertSigMul[U, P, (U, P0) %: ST0] { type Res = ST0 }

    // units do not match, so insert into tail of the signature
    transparent inline given [U, P, U0, P0, ST0](using une: U =/= U0, re: InsertSigMul[U, P, ST0]):
            InsertSigMul[U, P, (U0, P0) %: ST0] =
        new InsertSigMul[U, P, (U0, P0) %: ST0] { type Res = (U0, P0) %: re.Res }

@implicitNotFound("Could not prove ${T1} was not equal to ${T2}")
trait =/=[T1, T2]
object =/= :
    given [T1, T2](using NotGiven[T1 =:= T2]): =/=[T1, T2] =
        new =/=[T1, T2] {}
