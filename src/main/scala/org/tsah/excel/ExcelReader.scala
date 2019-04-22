package org.tsah.excel

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.ss.usermodel.{Cell => PoiExcellCell}
import org.tsah.excel.ExcelModel._

import scala.util.Try


object ExcelModel {
  sealed trait ExcelCell
  case object BlankCell extends ExcelCell
  case object ErrorCell extends ExcelCell
  case object FormulaCell extends ExcelCell
  case class BooleanCell(bool: Boolean) extends ExcelCell
  case class NumberCell(number: Double) extends ExcelCell
  case class DateCell(date: Date) extends ExcelCell
  case class StringCell(s: String) extends ExcelCell

  type ExcelLine = List[ExcelCell]
  type ExcelSheet = List[ExcelLine]

  def prettyPrintSheet(sheet: ExcelSheet): String =
    sheet map prettyPrintLine mkString "\n"

  def prettyPrintLine(line: ExcelLine): String =
    line map prettyPrintCell mkString ", "

  def prettyPrintCell(excelCell: ExcelCell): String = {
    excelCell match {
      case BlankCell => "BLANK"
      case ErrorCell => "ERROR"
      case FormulaCell => "FORMULA"
      case BooleanCell(b) => b.toString
      case NumberCell(n) => n.toString
      case DateCell(d) => new SimpleDateFormat().format(d)
      case StringCell(s) => s
    }
  }

}

object ExcelReader {

  val dateFormat = new SimpleDateFormat("d/M/y")

  def parseCell(cell: PoiExcellCell): ExcelCell = {
    import PoiExcellCell._

    cell.getCellType match {
      case CELL_TYPE_BLANK ⇒ BlankCell
      case CELL_TYPE_BOOLEAN ⇒ BooleanCell(cell.getBooleanCellValue)
      case CELL_TYPE_ERROR ⇒ ErrorCell
      case CELL_TYPE_FORMULA ⇒ FormulaCell
      case CELL_TYPE_NUMERIC ⇒
        if (org.apache.poi.ss.usermodel.DateUtil.isCellDateFormatted(cell)) {
          DateCell(cell.getDateCellValue)
        } else {
          NumberCell(cell.getNumericCellValue)
        }
      case CELL_TYPE_STRING ⇒
        val stringContent = cell.getRichStringCellValue.getString
        Try(dateFormat.parse(stringContent))
          .map(DateCell.apply)
          .getOrElse(StringCell(stringContent))
    }
  }

  def loadExcelSheet(file: File): ExcelSheet = {
    println(s"parsing file ${file.getName}")
    import scala.collection.JavaConverters._

    val fileIS = new java.io.FileInputStream(file)
    val wb = if (file.getName contains "xlsx") {
      new org.apache.poi.xssf.usermodel.XSSFWorkbook(fileIS)
    } else {
      new HSSFWorkbook(fileIS)
    }

    val worksheet = wb.getSheetAt(0)

    0.to(worksheet.getLastRowNum)
      .flatMap { i => Option(worksheet.getRow(i)) }
      .map { row =>
        row
          .cellIterator
          .asScala
          .toList
          .map(parseCell)
      }
      .toList
  }


  def printExcelFile(file: File): Unit = {
    loadExcelSheet(file)
      .map(_.mkString(","))
      .foreach(println)
  }

}

object Main extends App {
  import ExcelReader._

  val fileName = args(0)
  val showLongLines = args(1).toBoolean
  printExcelFile(new File(fileName))
}