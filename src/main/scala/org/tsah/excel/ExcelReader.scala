package org.tsah.excel

import org.apache.poi.ss.usermodel.Cell

object ExcelReader {
  def cellToString(cell: Cell): String = {
    import Cell._

    cell.getCellType match {
      case CELL_TYPE_BLANK ⇒ ""
      case CELL_TYPE_BOOLEAN ⇒ cell.getBooleanCellValue.toString
      case CELL_TYPE_ERROR ⇒ "ERROR"
      case CELL_TYPE_FORMULA ⇒ "FORMULA"
      case CELL_TYPE_NUMERIC ⇒
        if (org.apache.poi.ss.usermodel.DateUtil.isCellDateFormatted(cell)) {
          cell.getDateCellValue.toString
        } else {
          cell.getNumericCellValue.toString
        }
      case CELL_TYPE_STRING ⇒ cell.getRichStringCellValue.getString
    }
  }

  def getStringList(fileName: String): List[Vector[String]] = {
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
          .toVector
          .map(cellToString)
      }
        .map(
          _.map(
            _.replaceAll(",", " ")
          )
        )
        .toList
  }


  def printExcelFile(fileName: String): Unit = {
    getStringList(fileName)
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