package com.example

import cats.effect.IO

trait Product {
  val name: String

  val price: Double
}

trait Result

trait ShoppingCart {
  def add(products: Seq[(Product, Int)]): IO[Result]

  def cartSubtotal: IO[Double]

  def taxPayable: IO[Double]

  def totalPayable: IO[Double]
}
