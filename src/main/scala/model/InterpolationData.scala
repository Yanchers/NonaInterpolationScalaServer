package model

import cats.effect.IO
import org.http4s.circe._
import io.circe.generic.auto._
import org.http4s.EntityDecoder


object InterpolationData {
  case class LinearInterpolationRequest(zeta: Double, myu: Double)
  case class BilinearInterpolationThresholds(minMyu: Double, maxMyu: Double, minZeta: Double, maxZeta: Double)
}
object InterpolationErrors {
  sealed trait InterpolationError extends Exception
  case class InterpolationOutOfBoundsError(myu: Double, zeta: Double) extends InterpolationError
}

object DataDecoders {
  import InterpolationData._

  implicit val linearInterpolationRequest: EntityDecoder[IO, LinearInterpolationRequest] = jsonOf[IO, LinearInterpolationRequest]
}
