
import scala.concurrent.ExecutionContext
import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import fs2.Stream
import fs2.io.file.Files
import org.apache.poi.ss.usermodel.{Cell, CellType, Workbook, WorkbookFactory}
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.multipart.Multipart
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.{CORS, Logger}
import io.circe.syntax._
import org.http4s.circe._

import scala.jdk.CollectionConverters._
import model.InterpolationData._
import model.DataDecoders._
import model.InterpolationErrors.InterpolationOutOfBoundsError

object Http4sServer extends IOApp {
  private val excelFilesDirectoryPath = fs2.io.file.Path(System.getProperty("user.dir") + "/files")
  def excelRoutes(): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO] {}
    import dsl._

    HttpRoutes.of {
      case GET -> Root / "files" =>
        Files[IO].list(excelFilesDirectoryPath)
          .map(_.fileName.toString)
          .compile.toList
          .flatMap(l => Ok(l.asJson))
      case req@POST -> Root / "upload" =>
        val fileWriteIO = for {
          mp <- req.as[Multipart[IO]]
          part = mp.parts.head
          target = excelFilesDirectoryPath / fs2.io.file.Path(part.filename.get)
          _ <- {
            val filename = part.filename.get
            val ext = filename.substring(filename.lastIndexOf(".") + 1)
            println(s"File extension: $ext")
            if (ext == "xlsx" || ext == "xls")
              IO(())
            else
              IO.raiseError(new Exception("Invalid file extension."))
          }
          _ <- part.body.through(Files[IO].writeAll(target))
            .compile.drain
        } yield part.filename.get
        fileWriteIO
          .flatMap(fileName => Ok(s"Uploaded file: $fileName"))
          .handleErrorWith {
            error =>
              IO.println(s"Exception occurred: $error.\n${error.getStackTrace.mkString("\n")}") *>
                InternalServerError("Something happened")
          }

      case req @ POST -> Root / "process" / fileName =>
        implicit val cellNumericOrdering: Ordering[Cell] = (x: Cell, y: Cell) => {
          if (x.getNumericCellValue > y.getNumericCellValue) 1
          else if (x.getNumericCellValue < y.getNumericCellValue) -1
          else 0
        }
        val excelProcessing = for {
          linearData <- req.as[LinearInterpolationRequest]
          _ <- IO.println(s"data: $linearData")
          file <- IO(new java.io.File((excelFilesDirectoryPath / fileName).absolute.toString))
          wbRes = Resource.fromAutoCloseable[IO, Workbook](IO(WorkbookFactory.create(file)))
          res <- wbRes.use { wb =>
            for {
              sheet <- IO(wb.getSheetAt(0))
              myuRow <- IO {
                sheet.asScala.head.asScala
                  .filter(_.getCellType == CellType.NUMERIC)
                  .toList
              }
              myuThresholds <- IO {
                myuRow.partition(_.getNumericCellValue < linearData.myu) match {
                  case (before, after) =>
                    (before.maxOption.getOrElse(myuRow.head), after.minOption.getOrElse(myuRow.last))
                }
              }
              zetaCol <- IO {
                sheet.asScala
                  .flatMap(_.asScala
                    .filter(c => c.getCellType == CellType.NUMERIC && c.getColumnIndex == 0))
              }
              zetaThresholds <- IO {
                zetaCol.partition(_.getNumericCellValue < linearData.zeta) match {
                  case (before, after) => (before.maxOption.getOrElse(zetaCol.head), after.minOption.getOrElse(zetaCol.last))
                }
              }
              (myuMin, myuMax) = myuThresholds
              (zetaMin, zetaMax) = zetaThresholds
              _ <- IO.println(s"Myu threshold: $myuThresholds") *> IO.println(s"Zeta threshold: $zetaThresholds")
              _ <- {
                if (myuMin.getNumericCellValue == myuMax.getNumericCellValue ||
                  zetaMin.getNumericCellValue == zetaMax.getNumericCellValue
                )
                  IO.raiseError(InterpolationOutOfBoundsError(myuMin.getNumericCellValue, zetaMin.getNumericCellValue))
                else IO.pure()
              }
              res <- IO {
                val zetaMinRow = sheet.getRow(zetaMin.getRowIndex).asScala
                val zetaMaxRow = sheet.getRow(zetaMax.getRowIndex).asScala

                val myuMinValue = myuMin.getNumericCellValue
                val myuMaxValue = myuMax.getNumericCellValue

                val zetaMinValue = zetaMin.getNumericCellValue
                val zetaMaxValue = zetaMax.getNumericCellValue

                val zetaMinMyuMinValue = zetaMinRow
                  .find(_.getColumnIndex == myuMin.getColumnIndex).get.getNumericCellValue
                val zetaMinMyuMaxValue = zetaMinRow
                  .find(_.getColumnIndex == myuMax.getColumnIndex).get.getNumericCellValue

                val zetaMaxMyuMinValue = zetaMaxRow
                  .find(_.getColumnIndex == myuMin.getColumnIndex).get.getNumericCellValue
                val zetaMaxMyuMaxValue = zetaMaxRow
                  .find(_.getColumnIndex == myuMax.getColumnIndex).get.getNumericCellValue

                val myuZetaMinValue = zetaMinMyuMinValue +
                  (linearData.myu - myuMinValue) *
                    ((zetaMinMyuMaxValue - zetaMinMyuMinValue) / (myuMaxValue - myuMinValue))
                val myuZetaMaxValue = zetaMaxMyuMinValue +
                  (linearData.myu - myuMinValue) *
                    ((zetaMaxMyuMaxValue - zetaMaxMyuMinValue) / (myuMaxValue - myuMinValue))
                val result = myuZetaMinValue +
                  (linearData.zeta - zetaMinValue) *
                    ((myuZetaMaxValue - myuZetaMinValue) / (zetaMaxValue - zetaMinValue))

                result
              }
            } yield res
          }
        } yield res

        excelProcessing
          .flatTap(IO.println)
          .flatMap(r => Ok(r.toString))
          .handleErrorWith {
            case InterpolationOutOfBoundsError(myu, zeta) =>
              IO.println(s"InterpolationOutOfBoundsError: $myu, $zeta.") *>
                InternalServerError("Одно или несколько значений вне таблицы.")
            case error =>
              IO.println(s"Exception occurred: $error.\n${error.getStackTrace.mkString("\n")}") *>
                InternalServerError("Something happened")
          }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val apis = Router(
      "/api/excel" -> excelRoutes
    ).orNotFound

    val appWithLogger = Logger.httpApp(logHeaders = true, logBody = true)(apis)
    val appWithCors = CORS.policy.withAllowOriginAll(appWithLogger)

    Files[IO].exists(excelFilesDirectoryPath)
      .flatMap(b => if (b) IO.pure() else Files[IO].createDirectory(excelFilesDirectoryPath)) *>
    BlazeServerBuilder[IO]
      .bindHttp(8080, "26.29.14.210")
      .withHttpApp(appWithCors)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)

  }
}
