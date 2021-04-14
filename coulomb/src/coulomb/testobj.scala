package coulomb

object test:
    import coulomb.*

//    inline def addtest(q1: Quantity[Int, Second], q2: Quantity[Int, Second]): Quantity[Int, Second] =
//        q1 + (q2)

    // using summonInline and summonFrom makes 'inline' keyword 'viral', if
    // defining functions having type parameters
    inline def addTest[V, U](q1: Quantity[V, U], q2: Quantity[V, U]): Quantity[V, U] =
        q1 + q2

/*
    val lhs = 3.withUnit[Second]
    val rhs = 5.withUnit[Second]
    val zzz = lhs + rhs

    val www = addTest(lhs, rhs)
*/


    val t = 4.withUnit[Second]
    val v = t.vg

end test
