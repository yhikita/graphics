package com.ruimo.graphics.adapter

import scala.sys.process.Process
import java.nio.file.{Files, Path}

import scala.language.postfixOps
import com.ruimo.graphics.twodim.Degree
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object RotateImage {
  val logger = LoggerFactory.getLogger(getClass)

  def perform(img: Path, radianToRotate: Double): Int = {
    val rotatedFile = Files.createTempFile(null, ".png")
    val rc = (
      Process(
        "convert -rotate " + Degree.fromRadian(radianToRotate) +
        " " + img.toAbsolutePath + " " + rotatedFile.toAbsolutePath
      ) run
    ) exitValue()

    if (rc == 0) {
      logger.info("Rotate " + img.toAbsolutePath + " success.")
      Files.delete(img)
      Files.move(rotatedFile, img)
      rc
    }
    else {
      logger.info("Rotate " + img.toAbsolutePath + " failed. rc = " + rc)
      Files.delete(rotatedFile)
      rc
    }
  }
}
