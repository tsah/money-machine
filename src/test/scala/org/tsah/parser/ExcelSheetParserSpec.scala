package org.tsah.parser

import java.util.Date

import org.scalatest.FlatSpec
import org.tsah.excel.ExcelModel._
import org.tsah.model.Transaction
import org.tsah.model.Transaction.{apply => _, _}
import org.tsah.parser.ExcelSheetParser._


class ExcelSheetParserSpec extends FlatSpec {
  import ExcelSheetDSL._

  "The ExcelSheet parser" should "produce empty output for empty input" in {
    val emptyInput = List()
    val output = ExcelSheetParser().parse(emptyInput)
    assert(output.isEmpty)
  }

  it should "drop empty lines" in {
    val input = List(List())
    val output = ExcelSheetParser().parse(input)
    assert(output.isEmpty)
  }

  it should "produce parse errors for non empty lines if no header line is present" in {
    val input =
      List(
        List(StringCell("a"), StringCell("b"), StringCell("c")),
        List(StringCell("a"), NumberCell(1)),
        List(StringCell("a"), StringCell("b"), StringCell("c"), NumberCell(1))
      )

    val output = ExcelSheetParser().parse(input)

    val expectedOutput = input.map(LineParseFailed(_))
    assert(expectedOutput == output)
  }

  it should "try to parse a header line if it's made of strings and longer than 4 cells" in {
    val input =
      List(
        List(StringCell("a"), StringCell("b"), StringCell("c"), StringCell("d"))
      )
    val output = ExcelSheetParser().parse(input)
    val expected = input.map(HeaderLineParseFailed)
    assert(output == expected)
  }

  it should "parse a header line given an appropriate field dict" in {
    val input = List(List(StringCell("Account"), StringCell("TransactionDate"), StringCell("ChargeDate"), StringCell("Title")))
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title)
    val output = new ExcelSheetParser(dict).parse(input)
    assert(output == List(HeaderLine(List(Account, TransactionDate, ChargeDate, Title))))
  }

  it should "Fail to parse a short line after a good header line" in {
    val shortLine = line("account", new Date(), new Date())
    val input: ExcelSheet = sheet(
      line("Account", "TransactionDate", "ChargeDate", "Title"),
      shortLine
    )
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title, "PositiveBalance" -> PositiveBalance)
    val output = new ExcelSheetParser(dict).parse(input)
    val expectedOutput = List(
      HeaderLine(List(Account, TransactionDate, ChargeDate, Title)),
      LineParseFailed(shortLine))
    assert(output == expectedOutput)
  }

  it should "Fail to parse a bad transaction line after a good header line" in {
    val badTransactionLine = line("account", new Date(), new Date(), 1.1, 1.1)
    val input: ExcelSheet = sheet(
      line("Account", "TransactionDate", "ChargeDate", "Title", "PositiveBalance"),
      badTransactionLine
    )
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title, "PositiveBalance" -> PositiveBalance)
    val output = new ExcelSheetParser(dict).parse(input)
    val expectedOutput = List(
      HeaderLine(List(Account, TransactionDate, ChargeDate, Title, PositiveBalance)),
      TransactionParseFailed(badTransactionLine))
    assert(output == expectedOutput)
  }

  it should "Parse a good transaction line after a good header line" in {
    val someDate = new Date()
    val goodLine = line("account", someDate, someDate, "title", 1.0)
    val input = sheet(
      line("Account", "TransactionDate", "ChargeDate", "Title", "PositiveBalance"),
      goodLine
    )
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title, "PositiveBalance" -> PositiveBalance)
    val output = new ExcelSheetParser(dict).parse(input)
    val expectedOutput = List(
      HeaderLine(List(Account, TransactionDate, ChargeDate, Title, PositiveBalance)),
      ParseSuccess(Transaction("account", someDate, someDate, "title", 1.0, 0.0, None, None).get)
    )
    assert(output == expectedOutput)
  }
}

object ExcelSheetDSL {
  def string(s: String): StringCell = StringCell(s)
  def number(n: Double): NumberCell = NumberCell(n)
  def blank: ExcelCell = BlankCell
  def error: ExcelCell = ErrorCell
  def formula: ExcelCell = FormulaCell
  def bool(b: Boolean) = BooleanCell(b)
  def date(d: Date) = DateCell(d)

  def line(cells: ExcelCell*): ExcelLine = cells.toList

  def sheet(lines: ExcelLine*): ExcelSheet = lines.toList

  implicit def string2stringCell(s: String): StringCell = string(s)
  implicit def number2numberCell(n: Double): NumberCell = number(n)
  implicit def bool2boolCell(b: Boolean): BooleanCell = bool(b)
  implicit def date2dateCell(d: Date): DateCell = date(d)
}
