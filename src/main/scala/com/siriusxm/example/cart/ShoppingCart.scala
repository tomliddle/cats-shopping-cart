package com.siriusxm.example.cart

import cats.effect.IO

case class Product(name: String, price: Double)

case class Result(products: Seq[(Product, Long)])

trait ShoppingCart {
  def add(products: Seq[(Product, Long)]): IO[Result]

  def subtotal: IO[Double]

  def taxPayable: IO[Double]

  def totalPayable: IO[Double]
}

class InMemoryShoppingCart extends ShoppingCart {
  override def add(products: Seq[(Product, Long)]): IO[Result] = ???

  override def subtotal: IO[Double] = ???

  override def taxPayable: IO[Double] = ???

  override def totalPayable: IO[Double] = ???
}
