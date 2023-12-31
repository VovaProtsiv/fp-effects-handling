package exercise

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.std._

import scala.concurrent.duration.DurationInt
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.io.File
import java.awt.color.ColorSpace
import java.awt.image.ColorConvertOp

object QueueExercise extends IOApp {
  case class ImageInfo(filepath: String, image: BufferedImage)

  def processImage(imageInfo: ImageInfo): ImageInfo = {
    val colorOp = new ColorConvertOp(ColorSpace.getInstance(ColorSpace.CS_GRAY), null)
    val processedImage = colorOp.filter(imageInfo.image, imageInfo.image)
    imageInfo.copy(image = processedImage)
  }

  def saveImage(image: ImageInfo): IO[Unit] = {
    IO.blocking {
      val fp = image.filepath
      val newPath = s"${fp.substring(0, fp.length - 4)}_processed.jpg"
      ImageIO.write(image.image, "jpg", new File(s"$newPath"))
    }.void
  }

  // please make sure directory exists
  def loadImages(directory: String): IO[List[ImageInfo]] = {
    for {
      dir <- IO.blocking(new File(directory))
      files <- IO.blocking(dir.listFiles.toList.filter(f => f.isFile && f.getName.endsWith(".jpg")))
      images <- files.parTraverse(f => IO.blocking(ImageInfo(f.getAbsolutePath, ImageIO.read(f))))
    } yield images
  }

  // TODO: Take a processed image from the and save it to the corresponding file
  def imageSaver(
                  processedImageQueue: Queue[IO, ImageInfo]
                ): IO[Unit] = {
    IO.println("The image saver was created") *>
      processedImageQueue.take.flatMap(imageInfo => {
        IO.println(s"Saving the image: ${imageInfo.filepath}") *>
          saveImage(imageInfo)
      })

  }

  // TODO: Take a raw image from the queue, process it and put it in the processed queue
  def imageProcessor(
                      rawImageQueue: Queue[IO, ImageInfo],
                      processedImageQueue: Queue[IO, ImageInfo]
                    ): IO[Unit] = {
    IO.println("The image processor was created") *>
      rawImageQueue.take.flatMap {
        rawImage =>
          IO.println(s"Processing the image: ${rawImage.filepath}") *>
            processedImageQueue.offer(processImage(rawImage))
      }
  }

  // TODO: Load images from the dir and put them in the queue
  def imageLoader(
                   srcDirectory: String,
                   rawImageQueue: Queue[IO, ImageInfo]
                 ): IO[Unit] = {
    IO.println(s"Loading the images... from $srcDirectory") *>
      loadImages(srcDirectory).flatMap {
        images =>
          IO.println(s"Loaded ${images.length} images") *>
            images.parTraverse_(rawImageQueue.offer)
      }

  }

  // TODO: Create the loaders, savers and processors and get them all running!
  def start(
             sourceDirs: List[String],
             noProcessors: Int,
             noSavers: Int
           ): IO[Unit] = {
    Queue.unbounded[IO, ImageInfo].flatMap { rawImagesQueue =>
      Queue.unbounded[IO, ImageInfo].flatMap { processedImagesQueue =>
        val loaders = sourceDirs.map(dir => imageLoader(dir, rawImagesQueue))
        val processors = List.range(0, noProcessors).map(_ => imageProcessor(rawImagesQueue, processedImagesQueue))
        val savers = List.range(0, noSavers).map(_ => imageSaver(processedImagesQueue))
        (loaders ++ processors ++ savers).parSequence_
      }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val dirs = List("kittens", "puppies")
    start(dirs, 16, 16).timeoutTo(30.seconds, IO.unit).as(ExitCode.Success)
  }
}