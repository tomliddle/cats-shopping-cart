import cats.effect.IO
import com.comcast.ip4s.IpLiteralSyntax
import com.siriusxm.example.cart.Product
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import org.http4s.HttpRoutes
import org.http4s.Method.GET
import org.http4s.Uri.Path.Root
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router

object ProductRepoServerMock {

  val cheerios   = Product("cheerios", 1.2)
  val cornflakes = Product("cornflakes", 1.3)
  val frosties   = Product("frosties", 1.8)
  val shreddies  = Product("shreddies", 2.2)
  val weetabix   = Product("weetabix", 1.0)

  val service = HttpRoutes.of[IO] {
    case GET -> Root / "cheerios.json"   => Ok(cheerios.asJson)
    case GET -> Root / "cornflakes.json" => Ok(cornflakes.asJson)
    case GET -> Root / "shreddies.json"  => Ok(shreddies.asJson)
    case GET -> Root / "weetabix.json"   => Ok(weetabix.asJson)
    case GET -> Root / "frosties.json"   => Ok(frosties.asJson)
  }

  val httpApp = Router("" -> service).orNotFound

  val server = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8081")
    .withHttpApp(httpApp)
    .build
}
