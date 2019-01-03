package org.tsah.excel

import java.util.Date

import org.apache.poi.ss.usermodel.{Cell => PoiExcellCell}
import org.tsah.excel.ExcelModel._


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
}

object ExcelReader {

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
      case CELL_TYPE_STRING ⇒ StringCell(cell.getRichStringCellValue.getString)
    }
  }

  def loadExcelSheet(fileName: String): ExcelSheet = {
    println(s"parsing file $fileName")
    import scala.collection.JavaConverters._

    val fileIS = new java.io.FileInputStream(fileName)
    val wb = new org.apache.poi.xssf.usermodel.XSSFWorkbook(fileIS)
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


  def printExcelFile(fileName: String): Unit = {
    loadExcelSheet(fileName)
      .map(_.mkString(","))
      .foreach(println)
  }

}

object Main extends App {
  import ExcelReader._

  val fileName = args(0)
  val showLongLines = args(1).toBoolean
  printExcelFile(fileName)
}