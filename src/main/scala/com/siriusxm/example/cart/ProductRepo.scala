package com.siriusxm.example.cart

import cats.effect.IO
import io.circe.generic.auto._
import org.http4s.circe.jsonOf
import org.http4s.ember.client.EmberClientBuilder

trait ProductRepo {
  def retrieveProduct(product: String): IO[Product]
}

// e.g. s"https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/$product.json"
class ProductRepoImpl(url: String) extends ProductRepo {
  implicit val userDecoder = jsonOf[IO, Product]

  override def retrieveProduct(product: String): IO[Product] =
    EmberClientBuilder.default[IO].build.use(client => client.expect(s"$url$product.json"))
}
