package com.ruimo.graphics.adapter

import com.ruimo.scoins.Base64
import java.awt.image.BufferedImage
import java.nio.charset.{Charset, StandardCharsets}

import scala.sys.process.Process
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

import scala.language.postfixOps
import com.ruimo.scoins.PathUtil.withTempFile
import com.ruimo.scoins.Formatting.commaEdit

import org.slf4j.Logger
import org.slf4j.LoggerFactory

case class OcrResult(
  text: String,
  base64Img: String,
  rawText: String
) {
  lazy val num: String = OcrResult.normalize(text)
  lazy val commaEditted: OcrResult = copy(
    text = commaEdit(text.filter(Ocr.isDigit))
  )
}

object OcrResult {
  def apply(text: String, base64Img: String): OcrResult = OcrResult(
    text, base64Img, text
  )

  def normalize(text: String): String = {
    val noComma = text.filter(Ocr.isDigit)
    val idx = noComma.indexOf('-')
    if (idx < 0) noComma else "-" + noComma.substring(0, idx) + noComma.substring(idx + 1)
  }
}

object Ocr {
  val logger = LoggerFactory.getLogger(getClass)

  def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9' || c == '-'

  def perform(img: BufferedImage, charset: Charset = StandardCharsets.UTF_8, option: String): OcrResult =
    withTempFile(prefix = None, suffix = Some(".png")) { imgFile =>
      withTempFile(prefix = None, suffix = Some(".txt")) { txtFile =>
        ImageIO.write(img, "png", imgFile.toFile)
        perform(imgFile, txtFile, option)
        val result = new String(Files.readAllBytes(txtFile), charset)
        logger.info("Ocr result: '" + result + "'")
        OcrResult(result, Base64.encode(imgFile))
      }.get
    }.get

  def perform(imageFile: Path, ocrFile: Path, option: String): Int = {
    val cmd = "tesseract " + imageFile.toAbsolutePath + " stdout -psm 7 " + option
    logger.info("Invoking [" + cmd + "]")
    (Process(cmd) #> ocrFile.toFile run) exitValue()
  }
}
