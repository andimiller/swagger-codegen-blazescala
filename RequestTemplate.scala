import org.http4s._
import shapeless.Coproduct
import io.circe._

trait RequestTemplate[I, O] {

  type Errors <: Coproduct
  // Opaque for now. Might need Coproduct LiftAll
  def errorDecoder(status: Int): Decoder[Errors]

  def body: I
  def maybeBody: Option[I]
  def queryParams: Map[String, String]
  def headerParams: Map[String, String]
  def scopeRequired: Option[String] = None
  def relativePath: String
  def httpMethod: Method
}

object RequestTemplate {
  type Aux[I, O, E] = RequestTemplate[I, O] {
    type Errors = E
  }
}
