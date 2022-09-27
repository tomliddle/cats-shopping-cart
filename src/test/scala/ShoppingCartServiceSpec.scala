import ProductRepoServerMock._
import cats.effect.IO
import com.siriusxm.example.cart.InMemoryShoppingCartRepo.{TaxRate, TotalPayableRate}
import com.siriusxm.example.cart._
import munit.CatsEffectSuite

class ShoppingCartServiceSpec extends CatsEffectSuite {

  val shoppingCartService: IO[ShoppingCartService] =
    InMemoryShoppingCartRepo.instance.map(scr => new ShoppingCartService(scr, new ProductRepoImpl("http://localhost:8081/")))

  def toAddProduct(list: List[Item]): List[AddProduct] = list.map(i => AddProduct(i.product.title, i.count))

  val myFixture = ResourceSuiteLocalFixture(
    "my-server",
    ProductRepoServerMock.server,
  )

  override def munitFixtures = List(myFixture)

  // Add
  test("can add a single product to the shopping cart") {
    val productList = List(Item(cheerios, 1L))
    for {
      cart   <- shoppingCartService
      _      <- cart.add(toAddProduct(productList))
      result <- cart.get
    } yield assertEquals(result, productList)
  }

  test("can add a list of the same products to shopping cart") {
    val productList = List(Item(cheerios, 10L))
    for {
      cart   <- shoppingCartService
      _      <- cart.add(toAddProduct(productList))
      result <- cart.get
    } yield assertEquals(result, productList)
  }

  test("can add a list of different products to the shopping cart") {
    val productList = List(Item(cheerios, 10L), Item(cornflakes, 15L), Item(frosties, 7L), Item(shreddies, 1L), Item(weetabix, 10L))
    for {
      cart   <- shoppingCartService
      _      <- cart.add(toAddProduct(productList))
      result <- cart.get
    } yield assertEquals(result, productList)
  }

  test("can add an empty list to the shopping cart") {
    val productList = List.empty
    for {
      cart   <- shoppingCartService
      _      <- cart.add(toAddProduct(productList))
      result <- cart.get
    } yield assertEquals(result, productList)
  }

  test("can add a large number of products of different types to the shopping cart (above 32k at least)") {
    val productList =
      List(Item(cheerios, 5000L), Item(cornflakes, 300000L), Item(frosties, 700L), Item(shreddies, 44000L), Item(weetabix, 10L))
    for {
      cart   <- shoppingCartService
      _      <- cart.add(toAddProduct(productList))
      result <- cart.get
    } yield assertEquals(result, productList)
  }

  test("can call add more than once for the correct value") {
    val productList =
      List(Item(cheerios, 5000L))
    for {
      cart   <- shoppingCartService
      _      <- cart.add(toAddProduct(productList))
      _      <- cart.add(toAddProduct(productList))
      result <- cart.get
    } yield assertEquals(result, List(Item(cheerios, 10000L)))
  }

  // Calculate cart subtotal
  test("can calculate the cart subtotal for a single product") {
    for {
      cart     <- shoppingCartService
      _        <- cart.add(List(AddProduct(cheerios.title, 1L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, BigDecimal(1.2))
  }

  test("can calculate the cart subtotal for a list of the same product") {
    for {
      cart     <- shoppingCartService
      _        <- cart.add(List(AddProduct(cheerios.title, 20L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, BigDecimal(24.0))
  }

  test("can calculate the cart subtotal for a list of different products") {
    for {
      cart     <- shoppingCartService
      _        <- cart.add(List(AddProduct(cheerios.title, 20L), AddProduct(shreddies.title, 10L), AddProduct(weetabix.title, 5L)))
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, BigDecimal(24.0 + 22.0 + 5.0))
  }

  test("can calculate the cart subtotal for an empty cart") {
    for {
      cart     <- shoppingCartService
      _        <- cart.add(List.empty)
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, BigDecimal(0.0))
  }

  test("can calculate the cart subtotal for a large number of different products") {
    for {
      cart <- shoppingCartService
      _ <- cart.add(
        List(
          AddProduct(cheerios.title, 5000L),
          AddProduct(cornflakes.title, 300000L),
          AddProduct(frosties.title, 700L),
          AddProduct(shreddies.title, 44000L),
          AddProduct(weetabix.title, 10L),
        ),
      )
      subtotal <- cart.subtotal
    } yield assertEquals(subtotal, BigDecimal(494070.00)) //1.2 * 5000 + 1.3 * 300000 + 1.8 * 700 + 2.2 * 44000 + 1.0 * 10)
  }

  // Calculate tax
  test("can calculate the cart tax for a single product") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List(AddProduct(cheerios.title, 1L)))
      tax  <- cart.taxPayable
    } yield assertEquals(tax, TaxRate * 1.2)
  }

  test("can calculate the cart tax for a list of the same product") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List(AddProduct(cheerios.title, 10L)))
      tax  <- cart.taxPayable
    } yield assertEquals(tax, TaxRate * 12)
  }

  test("can calculate the cart tax for a list of different products") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List(AddProduct(cheerios.title, 10L), AddProduct(weetabix.title, 10L), AddProduct(frosties.title, 5L)))
      tax  <- cart.taxPayable
    } yield assertEquals(tax, TaxRate * (1.2 * 10 + 10 + 1.8 * 5))
  }

  test("can calculate the cart tax for an empty cart") {
    for {
      cart   <- shoppingCartService
      result <- cart.taxPayable
    } yield assertEquals(result, BigDecimal(0.0))

  }

  test("can calculate the cart tax for a large number of different products") {
    for {
      cart <- shoppingCartService
      _ <- cart.add(
        List(
          AddProduct(cheerios.title, 5000L),
          AddProduct(cornflakes.title, 300000L),
          AddProduct(frosties.title, 700L),
          AddProduct(shreddies.title, 44000L),
          AddProduct(weetabix.title, 10L),
        ),
      )
      _ <- cart.add(
        List(
          AddProduct(cheerios.title, 5000L),
          AddProduct(cornflakes.title, 300000L),
          AddProduct(frosties.title, 700L),
          AddProduct(shreddies.title, 44000L),
          AddProduct(weetabix.title, 10L),
        ),
      )
      subtotal <- cart.taxPayable
    } yield assertEquals(subtotal, 494070.0 * TaxRate * 2)
  }

  // Calculate total payable
  test("can calculate the cart total payable for a single product") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List(AddProduct(cheerios.title, 1L)))
      tax  <- cart.totalPayable
    } yield assertEquals(tax, TotalPayableRate * 1.2)
  }

  test("can calculate the cart total payable for a list of the same product") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List(AddProduct(cheerios.title, 10L)))
      tax  <- cart.totalPayable
    } yield assertEquals(tax, TotalPayableRate * 12)
  }

  test("can calculate the cart total payable for a list of different products") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List(AddProduct(cheerios.title, 10L), AddProduct(weetabix.title, 10L), AddProduct(frosties.title, 5L)))
      tax  <- cart.totalPayable
    } yield assertEquals(tax, TotalPayableRate * (1.2 * 10 + 10 + 1.8 * 5))
  }

  test("can calculate the cart total payable for an empty cart") {
    for {
      cart <- shoppingCartService
      _    <- cart.add(List.empty[AddProduct])
      tax  <- cart.totalPayable
    } yield assertEquals(tax, BigDecimal(0.0))
  }

  test("can calculate the cart total payable for a large number of different products") {
    for {
      cart <- shoppingCartService
      _ <- cart.add(
        List(
          AddProduct(cheerios.title, 5000L),
          AddProduct(cornflakes.title, 300000L),
          AddProduct(frosties.title, 700L),
          AddProduct(shreddies.title, 44000L),
          AddProduct(weetabix.title, 10L),
        ),
      )
      _ <- cart.add(
        List(
          AddProduct(cheerios.title, 5000L),
          AddProduct(cornflakes.title, 300000L),
          AddProduct(frosties.title, 700L),
          AddProduct(shreddies.title, 44000L),
          AddProduct(weetabix.title, 10L),
        ),
      )
      subtotal <- cart.totalPayable
    } yield assertEquals(subtotal, 494070.0 * TotalPayableRate * 2)
  }
}
