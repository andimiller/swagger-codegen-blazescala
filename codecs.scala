package argonautCodecs

import argonaut._
import java.time.format.DateTimeParseException
import org.http4s.Uri
import java.time._
import scalaz._, Scalaz._

object ArgonautCodecs {

  implicit val urlCodec: CodecJson[Uri] = CodecJson(
      (uri: Uri) => Json.jString(uri.toString),
      c =>
        c.as[String]
          .flatMap(str =>
                Uri
                  .fromString(str)
                  .fold(err => DecodeResult.fail(err.toString, c.history), DecodeResult.ok))
  )
  implicit val instantCodec: CodecJson[Instant] = CodecJson(
      (instant: Instant) => Json.jString(instant.toString),
      c =>
        c.as[String]
          .flatMap(
              str =>
                \/.fromTryCatchNonFatal(Instant.parse(str))
                  .orElse(
                      \/.fromTryCatchNonFatal(LocalDateTime.parse(str).toInstant(ZoneOffset.UTC)))
                  .fold(err =>
                          err match {
                        case e: DateTimeParseException => DecodeResult.fail(e.toString, c.history)
                        case e => throw e
                    },
                        DecodeResult.ok))
  )
}
