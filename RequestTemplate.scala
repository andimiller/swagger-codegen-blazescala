import argonaut._
import scalaz.concurrent.Task
import org.http4s.{DecodeResult => _, _}
import shapeless.Coproduct
import scalaz.{:+: => _, Coproduct => _, _}, Scalaz._

trait RequestTemplate[I, O] {

  type Errors <: Coproduct
  // Opaque for now. Might need Coproduct LiftAll
  def errorDecoder(status: Int): DecodeJson[Errors]

  def maybeBody: Option[I]
  def queryParams: Map[String, String]
  def headerParams: Map[String, String]
  def scopeRequired: Option[String]
  def relativePath: String
  def httpMethod: Method
}

object RequestTemplate {
  type Aux[I, O, E] = RequestTemplate[I, O] {
    type Errors = E
  }
}

trait GETRequestTemplate[I, O] extends RequestTemplate[Unit, O] {
  override final val maybeBody = None
  val httpMethod = Method.GET
}

trait PUTRequestTemplate[I, O] extends RequestTemplate[I, O] {
  override final def maybeBody = Some(body)
  def body: I
  val httpMethod = Method.PUT
}

trait POSTRequestTemplate[I, O] extends RequestTemplate[I, O] {
  override final def maybeBody = Some(body)
  def body: I
  val httpMethod = Method.POST
}

trait DELETERequestTemplate[I, O] extends RequestTemplate[Unit, Unit] {
  override final val maybeBody = None
  val httpMethod = Method.DELETE
}
