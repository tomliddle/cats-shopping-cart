import com.siriusxm.example.cart.{InMemoryShoppingCart, Product, Result}
import munit.CatsEffectSuite

class ShoppingCartSpec extends CatsEffectSuite {

  val cheerios   = Product("cheerios", 1.2)
  val cornflakes = Product("cornflakes", 1.3)
  val frosties   = Product("cornflakes", 1.8)
  val sheddies   = Product("cornflakes", 2.2)
  val weetabix   = Product("cornflakes", 1.0)

  val TaxRate          = 0.125
  val TotalPayableRate = 1 + TaxRate

  // Add
  test("can add a single product to the shopping cart") {
    val productList = Seq((cheerios, 1L))
    new InMemoryShoppingCart().add(productList).map(result => assertEquals(result, Result(productList)))
  }

  test("can add a list of the same products to shopping cart") {
    val productList = Seq((cheerios, 10L))
    new InMemoryShoppingCart().add(productList).map(result => assertEquals(result, Result(productList)))
  }

  test("can add a list of different products to the shopping cart") {
    val productList = Seq((cheerios, 10L), (cornflakes, 15L), (frosties, 7L), (sheddies, 1L), (weetabix, 10L))
    new InMemoryShoppingCart().add(productList).map(result => assertEquals(result, Result(productList)))
  }

  test("can add an empty list to the shopping cart") {
    val productList = Seq.empty
    new InMemoryShoppingCart().add(productList).map(result => assertEquals(result, Result(productList)))
  }

  test("can add a large number of products of different types to the shopping cart (above 32k at least)") {
    val productList = Seq((cheerios, 5000L), (cornflakes, 300000L), (frosties, 700L), (sheddies, 44000L), (weetabix, 10L))
    new InMemoryShoppingCart().add(productList).map(result => assertEquals(result, Result(productList)))
  }

  // Calculate cart subtotal
  test("can calculate the cart subtotal for a single product") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq((cheerios, 1L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, 1.2)
  }

  test("can calculate the cart subtotal for a list of the same product") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq((cheerios, 20L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, 24.0)
  }

  test("can calculate the cart subtotal for a list of different products") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq((cheerios, 20L), (sheddies, 10L), (weetabix, 5L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, 24.0 + 22.0 + 5.0)
  }

  test("can calculate the cart subtotal for an empty cart") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq.empty)
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, 0.0)
  }

  test("can calculate the cart subtotal for a large number of different products") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq((cheerios, 5000L), (cornflakes, 300000L), (frosties, 700L), (sheddies, 44000L), (weetabix, 10L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, 494070.0) //1.2 * 5000 + 1.3 * 300000 + 1.8 * 700 + 2.2 * 44000 + 1.0 * 10)
  }

  // Calculate tax
  test("can calculate the cart tax for a single product") {
    val cart = new InMemoryShoppingCart()
    for {
      _   <- cart.add(Seq((cheerios, 1L)))
      tax <- cart.taxPayable
    } yield assertEquals(tax, TaxRate * 1.2)
  }

  test("can calculate the cart tax for a list of the same product") {
    val cart = new InMemoryShoppingCart()
    for {
      _   <- cart.add(Seq((cheerios, 10L)))
      tax <- cart.taxPayable
    } yield assertEquals(tax, TaxRate * 12)
  }

  test("can calculate the cart tax for a list of different products") {
    val cart = new InMemoryShoppingCart()
    for {
      _   <- cart.add(Seq((cheerios, 10L), (weetabix, 10L), (frosties, 5L)))
      tax <- cart.taxPayable
    } yield assertEquals(tax, TaxRate * (1.2 * 10 + 10 + 1.8 * 5))
  }

  test("can calculate the cart tax for an empty cart") {
    new InMemoryShoppingCart().taxPayable.map(result => assertEquals(result, 0.0))
  }

  test("can calculate the cart tax for a large number of different products") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq((cheerios, 5000L), (cornflakes, 300000L), (frosties, 700L), (sheddies, 44000L), (weetabix, 10L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, 494070.0 * TaxRate)
  }

  // Calculate total payable
  test("can calculate the cart total payable for a single product") {
    val cart = new InMemoryShoppingCart()
    for {
      _   <- cart.add(Seq((cheerios, 1L)))
      tax <- cart.totalPayable
    } yield assertEquals(tax, TotalPayableRate * 1.2)
  }

  test("can calculate the cart total payable for a list of the same product") {
    val cart = new InMemoryShoppingCart()
    for {
      _   <- cart.add(Seq((cheerios, 10L)))
      tax <- cart.totalPayable
    } yield assertEquals(tax, TotalPayableRate * 12)
  }

  test("can calculate the cart total payable for a list of different products") {
    val cart = new InMemoryShoppingCart()
    for {
      _   <- cart.add(Seq((cheerios, 10L), (weetabix, 10L), (frosties, 5L)))
      tax <- cart.totalPayable
    } yield assertEquals(tax, TotalPayableRate * (1.2 * 10 + 10 + 1.8 * 5))
  }

  test("can calculate the cart total payable for an empty cart") {
    new InMemoryShoppingCart().totalPayable.map(result => assertEquals(result, 0.0))
  }

  test("can calculate the cart total payable for a large number of different products") {
    val cart = new InMemoryShoppingCart()
    for {
      _        <- cart.add(Seq((cheerios, 5000L), (cornflakes, 300000L), (frosties, 700L), (sheddies, 44000L), (weetabix, 10L)))
      subtotal <- cart.totalPayable
    } yield assertEquals(subtotal, 494070.0 * TotalPayableRate)
  }

}
