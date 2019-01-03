package org.tsah.parser

import org.tsah.excel.ExcelModel._
import org.tsah.model.Transaction
import org.tsah.model.Transaction._
import org.tsah.parser.ExcelSheetParser.{apply => _, _}

class ExcelSheetParser(fieldDict: Map[String, FieldType]) {

  def tryToParseHeaderLine(line: ExcelLine): Either[HeaderLineParseFailed, HeaderLine] = {
    val fields = line
      .collect {
        case s: StringCell => s
      }.flatMap { stringCell =>
        fieldDict.get(stringCell.s)
      }
    if (fields.size != line.size) {
      Left(HeaderLineParseFailed(line))
    } else if (! Transaction.MandatoryFields.forall{fields.contains}) {
      Left(HeaderLineParseFailed(line))
    } else {
      Right(HeaderLine(fields))
    }
  }


  def parse(sheet: ExcelSheet): List[ExcelParseResult] = {
    parse(sheet.filter(_.nonEmpty), None)
  }

  def parseTransactionLine(transactionLine: ExcelLine, headerline: HeaderLine): ExcelParseResult = {
    val asMap = headerline.header zip transactionLine toMap
    val possibleTransaction = for {
      account <- asMap.get(Account).collect{ case StringCell(s) => s }
      transactionDate <- asMap.get(TransactionDate).collect{ case DateCell(date) => date }
      chargeDate <- asMap.get(ChargeDate).collect{ case DateCell(date) => date }
      title <- asMap.get(Title).collect{ case StringCell(s) => s }
    } yield {
      val positiveBalance = asMap.get(PositiveBalance).collect{ case NumberCell(number) => number }.getOrElse(0.0)
      val negativeBalance = asMap.get(NegativeBalance).collect{ case NumberCell(number) => number }.getOrElse(0.0)
      val details = asMap.get(TransactionDetails).collect{ case StringCell(s) => s }
      val assignedType = asMap.get(AssignedType).collect{ case StringCell(s) => s }
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
      case (l::tail, Some(headerLine)) if l.size < headerLine.header.size =>
        LineParseFailed(l)::parse(tail, lastHeaderLine)
      case (l::tail, Some(headerLine)) =>
        parseTransactionLine(l, headerLine)::parse(tail, lastHeaderLine)
      case (l::tail, _) =>
        LineParseFailed(l)::parse(tail, lastHeaderLine)

    }
  }



  def candidateHeaderLine(l: ExcelLine): Boolean = {
    l.size >= Transaction.ManadatoryFieldsAmount && allStringCells(l)
  }

  def allStringCells(l: ExcelLine): Boolean = l.forall{
    case _ : StringCell => true
    case _ => false
  }
}

object ExcelSheetParser {

  def apply(): ExcelSheetParser = new ExcelSheetParser(Map())

  sealed trait ExcelParseResult
  case class ParseSuccess(t: Transaction) extends ExcelParseResult
  case class HeaderLine(header: List[FieldType]) extends ExcelParseResult
  trait ParseError extends ExcelParseResult
  case class LineParseFailed(line: ExcelLine) extends ParseError
  case class HeaderLineParseFailed(line: ExcelLine) extends ParseError
  case class TransactionParseFailed(line: ExcelLine) extends ParseError
}