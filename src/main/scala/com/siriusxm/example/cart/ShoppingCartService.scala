package com.siriusxm.example.cart

import cats.effect.IO
import cats.implicits.toTraverseOps
import com.siriusxm.example.cart.InMemoryShoppingCartRepo.{TaxRate, TotalPayableRate}

class ShoppingCartService(shoppingCartRepo: ShoppingCartRepo, productPriceRepo: ProductRepo) {

  val BigDecimalZero = BigDecimal(0.0)

  def add(addProduct: AddProduct): IO[Unit] = for {
    product <- productPriceRepo.retrieveProduct(addProduct.title)
    _       <- shoppingCartRepo.add(Item(product, addProduct.count))
  } yield ()

  def add(products: List[AddProduct]): IO[Unit] = products.traverse(add).void

  def get: IO[Seq[Item]] = shoppingCartRepo.get

  def subtotal: IO[BigDecimal] =
    get.map(f =>
      f.foldLeft(BigDecimalZero) { case (total, items) => total + BigDecimal(items.count * items.product.price) }
        .setScale(2, BigDecimal.RoundingMode.HALF_UP),
    )

  def taxPayable: IO[BigDecimal] = subtotal.map(_ * TaxRate)

  def totalPayable: IO[BigDecimal] = subtotal.map(_ * TotalPayableRate)

}
