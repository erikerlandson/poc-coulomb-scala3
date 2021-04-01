package coulomb

trait Addable[V]:
    def plus(x: V, y: V): V
  
given Addable[Int] with
    inline def plus(x: Int, y: Int): Int = x + y

trait Second

export quantity.Quantity as Quantity

extension[V] (v: V)
    inline def withUnit[U]: Quantity[V, U] = Quantity.lift[V, U](v)

extension[V, U] (ql: Quantity[V, U])
    inline def +(qr: Quantity[V, U])(using va: Addable[V]): Quantity[V, U] =
        Quantity.lift[V, U](va.plus(ql.value, qr.value))

object quantity:
    opaque type Quantity[V, U] = V

    // The only two methods I need in scope of the opaque type
    // are a way to lift raw values into a Quantity
    // and a way to extract raw values from a quantity

    // lift
    object Quantity:
        def lift[V, U](v: V): Quantity[V, U] = v
    end Quantity

    // extract
    extension[V, U] (ql: Quantity[V, U])
        def value: V = ql
end quantity
