
import scala.concurrent.ExecutionContext
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
import org.http4s.server.middleware.Logger

import scala.jdk.CollectionConverters._
import model.InterpolationData._
import model.DataDecoders._

object Http4sServer extends IOApp {
  def excelRoutes(): HttpRoutes[IO] = {
    val dsl = new Http4sDsl[IO] {}
    import dsl._

    HttpRoutes.of {
      case req@POST -> Root / "upload" =>
        val fileWriteIO = for {
          mp <- req.as[Multipart[IO]]
          part = mp.parts.head
          target = fs2.io.file.Path(part.filename.get)
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
          file <- IO(new java.io.File(part.filename.get))
          _ <- {
            if (!file.exists())
              IO.raiseError(new Exception("File does not exits."))
            else
              IO(())
          }
        } yield part.filename.get
        fileWriteIO
          .flatMap(fileName => Ok(s"Uploaded file: $fileName"))
          .handleErrorWith(e => IO.println(s"Error occurred: $e") *> InternalServerError(s"Something happened ${e.getMessage}"))

      case req @ POST -> Root / "process" / fileName =>
        implicit val cellNumericOrdering: Ordering[Cell] = (x: Cell, y: Cell) => {
          if (x.getNumericCellValue > y.getNumericCellValue) 1
          else if (x.getNumericCellValue < y.getNumericCellValue) -1
          else 0
        }
        val excelProcessing = for {
          linearData <- req.as[LinearInterpolationRequest]
          file <- IO(new java.io.File(fileName))
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
                  case (before, after) => (before.max, after.min)
                }
              }
              zetaCol <- IO {
                sheet.asScala
                  .flatMap(_.asScala
                    .filter(c => c.getCellType == CellType.NUMERIC && c.getColumnIndex == 0))
              }
              zetaThresholds <- IO {
                zetaCol.partition(_.getNumericCellValue < linearData.zeta) match {
                  case (before, after) => (before.max, after.min)
                }
              }
              (myuMin, myuMax) = myuThresholds
              (zetaMin, zetaMax) = zetaThresholds
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
          .flatMap(r => Ok(s"Processed file $fileName. Result: $r"))
          .handleErrorWith {
            error =>
              IO.println(s"Exception occurred: $error. ${error.getStackTrace.mkString("Array(", ", ", ")")}") *>
                InternalServerError("Something happened")
          }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val apis = Router(
      "/api/excel" -> excelRoutes
    ).orNotFound

    val finalApp = Logger.httpApp(logHeaders = true, logBody = true)(apis)

    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(finalApp)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)

  }
}
