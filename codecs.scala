import java.time.format.DateTimeParseException
import org.http4s.Uri
import java.time._
import io.circe._

object CirceCodecs {
  implicit val integerDecoder: Decoder[Integer] = Decoder[Int].map(i => new Integer(i.toString))
}
