package com.siriusxm.example.cart

import cats.effect.{IO, Ref}
import cats.syntax.traverse.toTraverseOps

case class AddProduct(title: String, count: Long)
case class Product(title: String, price: Double)
case class Item(product: Product, count: Long)

trait ShoppingCartRepo {
  def add(product: Item): IO[Unit]

  def add(items: List[Item]): IO[Unit]

  def get: IO[Seq[Item]]
}

object InMemoryShoppingCartRepo {
  val TaxRate: BigDecimal          = BigDecimal(0.125)
  val TotalPayableRate: BigDecimal = TaxRate + 1

  val instance: IO[InMemoryShoppingCartRepo] = Ref[IO].of(List.empty[Item]).map(new InMemoryShoppingCartRepo(_))
}

class InMemoryShoppingCartRepo(ref: Ref[IO, List[Item]]) extends ShoppingCartRepo {

  private def merge(items: List[Item], item: Item): List[Item] = {
    items match {
      case head :: tail if head.product == item.product => head.copy(count = head.count + item.count) :: tail
      case head :: tail                                 => head :: merge(tail, item)
      case Nil                                          => List(item)
    }
  }

  override def get: IO[List[Item]] = ref.get

  override def add(addProduct: Item): IO[Unit] = ref.update(merge(_, addProduct)).void

  override def add(items: List[Item]): IO[Unit] = items.traverse(add).void
}
