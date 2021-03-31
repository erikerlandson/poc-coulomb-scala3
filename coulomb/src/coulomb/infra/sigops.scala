package repro

trait SNil
trait %:[H, T]
trait Zero
trait Fail

type FilterZ[S] = S match
    case SNil => SNil
    case Zero %: t => FilterZ[t]
    case h %: t => h %: FilterZ[t]
    case Any => Fail
