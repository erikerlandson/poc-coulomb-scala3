package repro

class SNil
class %:[H, T]
class Zero
class Fail

type FilterZ[S] = S match
    case SNil => SNil
    case Zero %: t => FilterZ[t]
    case h %: t => h %: FilterZ[t]
    case Any => Fail
