package org.matthicks.pdf4s

import com.itextpdf.text.pdf.PdfContentByte
import com.itextpdf.text.Font
import scala.annotation.tailrec

/**
  * @author Matt Hicks <matt@outr.com>
  */
class MultiLine(generator: PDFWriter, content: PdfContentByte, var x: Double, width: Double, defaultLeading: Double) {
  var xOffset: Double = x
  private var currentLeading: Double = defaultLeading
  private val maxX = x + width

  @tailrec
  final def lineBreak(times: Int = 1): Unit = {
    generator.yOffset -= currentLeading
    currentLeading = defaultLeading
    xOffset = x
    generator.verifySpace(currentLeading)
    if (times > 1) {
      lineBreak(times - 1)
    }
  }

  def addSpace(horizontal: Double) = {
    xOffset += horizontal
    verifyBreak()
  }

  def measureLine(text: String, font: Font, kerning: Boolean, style: DrawStyle) = {
    var width = 0.0f
    val characters = text.map { c =>
      val (char, size) = style match {
        case DrawStyle.Normal => (c, font.getSize)
        case DrawStyle.SmallCaps => (c.toUpper, if (c.isLower) font.getSize * 0.7f else font.getSize)
        case DrawStyle.Subscript => (c, font.getSize * 0.7f)
        case DrawStyle.Superscript => (c, font.getSize * 0.5f)
      }
      val w = if (char == '\t') {
        font.getBaseFont.getWidthPoint(" ", size) * 4
      } else if (kerning) {
        font.getBaseFont.getWidthPointKerned(char.toString, size)
      } else {
        font.getBaseFont.getWidthPoint(char.toString, size)
      }
      width += w
      MeasuredCharacter(char, w, size)
    }.toList
    MeasuredLine(width, characters.map(mc => mc.c).mkString, characters)
  }

  def parseWords(text: String) = {
    var words = List.empty[String]
    val b = new StringBuilder
    for (c <- text) {
      if (c == ' ') {
        if (b.nonEmpty) {
          words = b.toString() :: words
          b.clear()
        }
      } else if (c == '\n') {
        if (b.nonEmpty) {
          words = b.toString() :: words
          b.clear()
        }
        words = "\n" :: words
      } else if (c == '\t') {
        if (b.nonEmpty) {
          words = b.toString() :: words
          b.clear()
        }
        words = "\t" :: words
      } else {
        b.append(c)
      }
    }
    if (b.nonEmpty) {
      words = b.toString() :: words
    }
    words.reverse
  }

  def measure(text: String, leading: Double, font: Font, kerning: Boolean, style: DrawStyle, breakOffset: Double) = {
    val words = parseWords(text)
    var lines = List.empty[MeasuredLine]
    val b = new StringBuilder
    var newX = xOffset
    var newY = generator.yOffset
    var previousMeasured = MeasuredLine(0.0f, "", Nil)
    words.foreach {
      case "\n" => {
        lines = previousMeasured :: lines
        b.clear()
        newX = x + breakOffset
        newY -= leading
      }
      case word => {
        if (b.isEmpty && newX == x + breakOffset) {
          b.append(word)
        } else {
          val line = if (b.isEmpty) {
            word
          } else {
            s"$b $word"
          }
          val measuredLine = measureLine(line, font, kerning, style)
          if (newX + measuredLine.width <= maxX) {
            b.append(' ')
            b.append(word)
          } else {
            // Line Break
            lines = previousMeasured :: lines
            b.clear()
            b.append(word)
            newX = x + breakOffset
            newY -= leading
          }
          previousMeasured = measuredLine
        }
      }
    }
    if (b.nonEmpty) {
      lines = measureLine(b.toString(), font, kerning, style) :: lines
      newY -= leading
    }
    lines = lines.reverse

    val height = leading * lines.size
    MultiLineMeasurement(lines, height.toFloat, leading.toFloat, font, kerning, style, breakOffset.toFloat)
  }

  def draw(text: String, leading: Double, font: Font, kerning: Boolean = true, style: DrawStyle = DrawStyle.Normal, breakOffset: Double = 0.0): List[MeasuredLine] = {
    val measurement = measure(text, leading, font, kerning, style, breakOffset)
    draw(measurement)
    measurement.lines
  }

  def draw(measurement: MultiLineMeasurement) = {
    //    content.setFontAndSize(measurement.font.getBaseFont, measurement.font.getSize)
    var first = true
    measurement.lines.foreach { line =>
      if (first) {
        first = false
      } else {
        xOffset = x + measurement.breakOffset
        generator.yOffset -= measurement.leading
      }
      content.beginText()
      val ascent = measurement.leading * 0.6666f
      line.characters.foreach { mc =>
        val y = measurement.style match {
          case DrawStyle.Normal => generator.yOffset - ascent
          case DrawStyle.SmallCaps => generator.yOffset - ascent
          case DrawStyle.Subscript => generator.yOffset - ascent
          case DrawStyle.Superscript => generator.yOffset - (ascent / 2.0f)
        }
        content.setFontAndSize(measurement.font.getBaseFont, mc.size)
        content.setTextMatrix(xOffset.toFloat, y.toFloat)
        if (mc.c == '\t') {
          // Nothing to draw
        } else if (measurement.kerning) {
          content.showTextKerned(mc.c.toString)
        } else {
          content.showText(mc.c.toString)
        }
        xOffset += mc.width
      }
      content.endText()
      if (generator.verifySpace(measurement.leading * 3.0f)) {
        xOffset = x + measurement.breakOffset // Reset x if page break occurred.
      }
    }
  }

  private def verifyBreak() = {
    if (xOffset - x > width) {
      lineBreak()
    }
  }
}

case class MultiLineMeasurement(lines: List[MeasuredLine], height: Float, leading: Float, font: Font, kerning: Boolean, style: DrawStyle, breakOffset: Float)

case class MeasuredCharacter(c: Char, width: Float, size: Float)

case class MeasuredLine(width: Float, text: String, characters: List[MeasuredCharacter])