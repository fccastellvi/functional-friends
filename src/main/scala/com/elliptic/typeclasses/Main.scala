package com.elliptic.typeclasses


object Main {

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val tenLists = List.tabulate(10, 10)(_ + _)

    import Combine.addZero
    val addResult = addUp(list)
    println(s"Result: $addResult")
    val addAddResult = addAddUp(tenLists)
    println(s"Result: $addAddResult")
  }

  // Use an implicit `Combine` argument
  def addUp[A](xs: List[A])(implicit c: Combine[A]): A = xs match {
    case Nil => c.base
    case x :: xs => c.combine(x, addUp(xs))
  }

  // Or add a `Combine` constraint when we don't need to use the methods directly
  def addAddUp[A: Combine](xss: List[List[A]]): A = addUp(xss.map(r => addUp(r)))

}

// Definitely not monoids ...
trait Combine[A] {
  val base: A
  def combine(a: A, b: A): A
}

object Combine {
  // We can have more than one implementation for a given type ...
  implicit val addZero: Combine[Int] = new Combine[Int] {
    override val base: Int = 0
    override def combine(a: Int, b: Int): Int = a + b
  }

  implicit val mulOne: Combine[Int] = new Combine[Int] {
    override val base: Int = 1
    override def combine(a: Int, b: Int): Int = a * b
  }

  // .. and we can pick our behaviour based on the type
  type ShoppingList = List[String]

  implicit val shoppingList: Combine[ShoppingList] = new Combine[ShoppingList] {
    override val base: ShoppingList = List()
    override def combine(a: ShoppingList, b: ShoppingList): ShoppingList = a ++ b
  }

  // We can use combine as a dependency when needed
  implicit def pairs[A: Combine, B: Combine]: Combine[(A, B)] = new Combine[(A, B)] {
    // Implicits can also be brought into scope manually
    private val aa = implicitly[Combine[A]]
    private val ab = implicitly[Combine[B]]

    override val base: (A, B) = (aa.base, ab.base)
    override def combine(a: (A, B), b: (A, B)): (A, B) = (aa.combine(a._1, b._1), ab.combine(a._2, b._2))
  }
}

// We can have more than one parameter ...
trait Coerce[A, B] {
  def coerce(a: A): B
}

object Coerce {
  implicit val isZero: Coerce[Int, Boolean] = (a: Int) => a == 0
  implicit val stringify: Coerce[Int, String] = (a: Int) => s"Int: $a"
}

// We can even declare internal associated types ...
trait Assoc[A] {
  type B
  def blah(a: A): B
}

object Assoc {
  implicit val stringExists = new Assoc[Int] {
    override type B = String
    def blah(a: Int): String = s"Hello world from $a"
  }

  implicit val booleanExists = new Assoc[Int] {
    override type B = Boolean
    override def blah(a: Int): Boolean = a == 0
  }
}
