package coulomb.infra

import scala.compiletime.ops.int._
import scala.compiletime.ops.boolean._
import scala.compiletime.ops.any._

trait SNil
trait %:[H, T]

type FilterZ1[S] = S match
    case SNil => SNil
    case 0 %: t => t
    case h %: t => h %: FilterZ1[t]

// shim to enforce singleton type inference
trait Shim[V <: Singleton]

type FilterZ2[S] = S match
    case SNil => SNil
    case Shim[0] %: t => t
    case Shim[h] %: t => h %: FilterZ2[t]

trait Alloc[T]
def alloc[T] = new Alloc[T] {}
