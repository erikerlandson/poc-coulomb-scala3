package coulomb

import scala.compiletime.{ summonInline, summonFrom }

trait Addable[V]:
    def plus(x: V, y: V): V
object Addable:
    given Addable[Int] with
        inline def plus(x: Int, y: Int): Int = x + y
    given Addable[Double] with
        inline def plus(x: Double, y: Double): Double = x + y
    given Addable[String] with
        inline def plus(x: String, y: String): String = x ++ y

trait Second

export quantity.Quantity as Quantity
export quantity.withUnit as withUnit

extension[V, U] (ql: Quantity[V, U])
    inline def +(qr: Quantity[V, U]): Quantity[V, U] =
        (summonInline[Addable[V]]).plus(ql.value, qr.value).withUnit[U]

extension[U] (ql: Quantity[Int, U])
    inline def +(qr: Quantity[Int, U]): Quantity[Int, U] =
        (summonInline[Addable[Int]]).plus(ql.value, qr.value).withUnit[U]

object quantity:
    opaque type Quantity[V, U] = V

    // The only two methods I need in scope of the opaque type
    // are a way to lift raw values into a Quantity
    // and a way to extract raw values from a quantity

    // lift
    object Quantity:
        def lift[V, U](v: V): Quantity[V, U] = v
        def lift[U](v: Int): Quantity[Int, U] = v
    end Quantity

    // extract
    extension[V, U](ql: Quantity[V, U])
        def value: V = ql
    extension[U](ql: Quantity[Int, U])
        def value: Int = ql

    extension[V](v: V)
        def withUnit[U]: Quantity[V, U] = v
    extension(v: Int)
        def withUnit[U]: Quantity[Int, U] = v

end quantity

/*
extension [V](inline v: V)
    inline def wu[U]: Quantity[V, U] = ${ macros.wuImpl[V, U]('v) }
*/

extension [V, U](inline q: Quantity[V, U])
    inline def vg: V = ${ macros.vgImpl[V, U]('q) }

object macros:
    import scala.quoted.*

    def vgImpl[V, U](qexpr: Expr[Quantity[V, U]])(using Type[V], Type[U], Quotes): Expr[V] =
        import quotes.reflect.*
        Console.withOut(java.io.FileOutputStream("/tmp/q.txt")) {
            println(s"q: ${qexpr.asTerm.show(using Printer.TreeStructure)}")
            val qt = TypeTree.of[Quantity[V, U]]
            println(s"qt: ${qt.show(using Printer.TreeStructure)}")
        }
        '{ ${qexpr}.value }

/*
    def wuImpl[V, U](vexpr: Expr[V])(using Type[V], Type[U], Quotes): Expr[Quantity[V, U]] =
        '{ vexpr }
*/
end macros
