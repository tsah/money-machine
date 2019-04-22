package org.tsah.parser

import org.tsah.excel.ExcelModel
import org.tsah.excel.ExcelModel._
import org.tsah.model.Transaction
import org.tsah.model.Transaction._
import org.tsah.parser.ExcelSheetParser.{apply => _, _}

class ExcelSheetParser(fieldDict: Map[String, FieldType]) {

  def tryToParseHeaderLine(line: ExcelLine): Either[HeaderLineParseFailed, HeaderLine] = {
    val fields = line
      .collect {
        case s: StringCell => s
      }.map { stringCell =>
        fieldDict.get(stringCell.s)
      }
    if (!Transaction.MandatoryFields.map(Some.apply).forall{fields.contains}) {
      Left(HeaderLineParseFailed(line))
    } else {
      Right(HeaderLine(fields))
    }
  }


  def parse(sheet: ExcelSheet): List[ExcelParseResult] = {
    parse(sheet.filter(_.nonEmpty), None)
  }

  def parseTransactionLine(transactionLine: ExcelLine, headerline: HeaderLine): ExcelParseResult = {
    val asMap = headerline.header
      .zip(transactionLine)
      .collect { case (Some(field), cell) => (field, cell) }
      .toMap
    val possibleTransaction = for {
      transactionDate <- asMap.get(TransactionDate).collect{ case DateCell(date) => date }
      title <- asMap.get(Title).collect{ case StringCell(s) => s }
    } yield {
      val positiveBalance = asMap.get(PositiveBalance).collect{ case NumberCell(number) => number }.getOrElse(0.0)
      val negativeBalance = asMap.get(NegativeBalance).collect{ case NumberCell(number) => number }.getOrElse(0.0)
      val details = asMap.get(TransactionDetails).collect{ case StringCell(s) => s }
      val assignedType = asMap.get(AssignedType).collect{ case StringCell(s) => s }
      val chargeDate = asMap.get(ChargeDate).collect{ case DateCell(date) => date }
      val account = asMap.get(Account).collect{ case StringCell(s) => s }.getOrElse("")
      Transaction(account, transactionDate, chargeDate, title, positiveBalance, negativeBalance, details, assignedType)
    }
    possibleTransaction.flatten match {
      case Some(transaction) => ParseSuccess(transaction)
      case _ => TransactionParseFailed(transactionLine)
    }
  }

  private def parse(sheet: ExcelSheet, lastHeaderLine: Option[HeaderLine]): List[ExcelParseResult] = {
    (sheet, lastHeaderLine) match {
      case (Nil, _) => Nil
      case (l::tail, _) if candidateHeaderLine(l) =>
        tryToParseHeaderLine(l) match {
          case Left(parseFailed) => parseFailed::parse(tail, lastHeaderLine)
          case Right(newHeaderLine) => newHeaderLine::parse(tail, Some(newHeaderLine))
        }
      case (l::tail, Some(headerLine)) =>
        parseTransactionLine(l, headerLine)::parse(tail, lastHeaderLine)
      case (l::tail, _) =>
        NoHeaderLine(l)::parse(tail, lastHeaderLine)

    }
  }



  def candidateHeaderLine(l: ExcelLine): Boolean = {
    l.size >= Transaction.MandatoryFieldsAmount && allStringCells(l)
  }

  def allStringCells(l: ExcelLine): Boolean = l.forall{
    case _ : StringCell => true
    case _ => false
  }
}

object ExcelSheetParser {

  def apply(): ExcelSheetParser = new ExcelSheetParser(Map())

  sealed trait ExcelParseResult
  final case class ParseSuccess(t: Transaction) extends ExcelParseResult
  final case class HeaderLine(header: List[Option[FieldType]]) extends ExcelParseResult
  sealed trait ParseError extends ExcelParseResult
  final case class LineTooShort(line: ExcelLine, expectedLenght: Int) extends ParseError
  final case class NoHeaderLine(line: ExcelLine) extends ParseError
  final case class HeaderLineParseFailed(line: ExcelLine) extends ParseError
  final case class TransactionParseFailed(line: ExcelLine) extends ParseError

  def detailedPrint(results: List[ExcelParseResult]): String = results.map(detailedPrint).mkString("\n")

  def detailedPrint(result: ExcelParseResult): String = result match {
    case ParseSuccess(t) => s"TRANSACTION: ${t.toCsvLine}"
    case HeaderLine(h) => s"HEADER: ${h.map(_.toString).mkString(", ")}"
    case LineTooShort(line, expectedLenght) => s"LINE TOO SHORT: ${ExcelModel.prettyPrintLine(line)}, expected size: $expectedLenght"
    case NoHeaderLine(line) => s"NO HEADER LINE: ${ExcelModel.prettyPrintLine(line)}"
    case HeaderLineParseFailed(l) => s"HEADER FAILURE: ${ExcelModel.prettyPrintLine(l)}"
    case TransactionParseFailed(l) => s"TRANSACTION FAILURE: ${ExcelModel.prettyPrintLine(l)}"
  }
}