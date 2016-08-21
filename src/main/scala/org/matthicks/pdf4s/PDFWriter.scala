package org.matthicks.pdf4s

import java.io.{File, FileOutputStream}

import com.itextpdf.text.pdf.{PdfContentByte, PdfReader, PdfWriter}
import com.itextpdf.text.{BaseColor, Document, Font, Rectangle}

import scala.annotation.tailrec

class PDFWriter(file: File,
                pageSize: Rectangle,
                breakAtBeginning: Boolean = false,
                breakAmount: Double = 5.0,
                margins: Margins = Margins()) {
  private val document = new Document(pageSize, margins.left.toFloat, margins.right.toFloat, margins.top.toFloat, margins.bottom.toFloat)
  private val output = new FileOutputStream(file)
  private val writer = PdfWriter.getInstance(document, output)
  document.open()
  private val content = writer.getDirectContent
  private val top = pageSize.getTop(margins.top.toFloat).toDouble
  private[pdf4s] var yOffset = top
  private val pageTop = pageSize.getTop.toDouble

  def addPDF(pdfFile: File, x: Double, y: Double): Unit = {
    val reader = new PdfReader(pdfFile.toURI.toURL)
    val page = writer.getImportedPage(reader, 1)
    content.addTemplate(page, x.toFloat, y.toFloat)
  }

  def drawText(text: String, font: Font, x: Double, style: DrawStyle = DrawStyle.Normal): (Double, Double) = {
    val ascent = font.getBaseFont.getAscentPoint(text, font.getSize)
    val descent = font.getBaseFont.getDescentPoint(text, font.getSize)

    var xOffset = x

    content.beginText()
    text.foreach { c =>
      val (char, size, y) = style match {
        case DrawStyle.Normal => (c, font.getSize, yOffset - ascent)
        case DrawStyle.SmallCaps => (c.toUpper, if (c.isLower) font.getSize * 0.7f else font.getSize, yOffset - ascent)
        case DrawStyle.Subscript => (c, font.getSize * 0.7f, yOffset - ascent)
        case DrawStyle.Superscript => (c, font.getSize * 0.5f, yOffset - (ascent / 2.0f))
      }
      val w = font.getBaseFont.getWidthPoint(char, size)
      content.setFontAndSize(font.getBaseFont, size)
      content.showTextAligned(PdfContentByte.ALIGN_LEFT, char.toString, xOffset.toFloat, y.toFloat, 0.0f)
      xOffset += w
    }
    content.endText()
    (xOffset, yOffset - ascent - descent)
  }

  def drawTextLine(text: String, font: Font, x: Double, style: DrawStyle = DrawStyle.Normal): Unit = {
    val (_, y) = drawText(text, font, x, style)
    yOffset = y
  }

  def drawTextPositioned(text: String, font: Font, alignment: Alignment, x: Double, y: Double = yOffset): Unit = {
    content.setColorFill(BaseColor.BLACK)
    content.beginText()
    content.setFontAndSize(font.getBaseFont, font.getSize)
    content.showTextAligned(alignment.value, text, x.toFloat, y.toFloat, 0.0f)
    content.endText()
  }

  def createMultiLine(x: Float, leading: Float, width: Float, text: String, font: Font)(before: Float => Unit) = {
    val ml = new MultiLine(this, content, x, width, leading)
    val measurement = ml.measure(text, leading, font, kerning = true, style = DrawStyle.Normal, breakOffset = 0.0f)
    before(measurement.height)
    ml.draw(measurement)
  }

  def createNextPage() = {
    document.newPage()
    yOffset = pageTop
  }

  @tailrec
  final def break(amount: Double = breakAmount, times: Int = 1): Unit = {
    if (breakAtBeginning || yOffset != pageTop) {   // Only break when not at the top of the page
      yOffset -= amount
      verifySpace(60.0)
      if (times > 1) {
        break(amount, times - 1)
      }
    }
  }

  def verifySpace(vertical: Double) = {
    if (yOffset < vertical) {
      createNextPage()
      true
    } else {
      false
    }
  }

  def saveAndClose() = {
    output.flush()
    document.close()
    output.close()
  }
}

case class Margins(left: Double, right: Double, top: Double, bottom: Double)

object Margins {
  def apply(): Margins = Margins(36.0, 36.0, 36.0, 36.0)
  def apply(value: Double): Margins = Margins(value, value, value, value)
}

sealed trait DrawStyle

object DrawStyle {
  case object Normal extends DrawStyle
  case object SmallCaps extends DrawStyle
  case object Superscript extends DrawStyle
  case object Subscript extends DrawStyle
}

sealed abstract class Alignment(val value: Int)

object Alignment {
  case object Left extends Alignment(PdfContentByte.ALIGN_LEFT)
  case object Center extends Alignment(PdfContentByte.ALIGN_CENTER)
  case object Right extends Alignment(PdfContentByte.ALIGN_RIGHT)
}