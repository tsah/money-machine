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
        List(StringCell("a"), StringCell("b")),
        List(StringCell("a"), NumberCell(1)),
        List(StringCell("a"), StringCell("b"), StringCell("c"), NumberCell(1))
      )

    val output = ExcelSheetParser().parse(input)

    val expectedOutput = input.map(NoHeaderLine(_))
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
    assert(output == List(HeaderLine(List(Account, TransactionDate, ChargeDate, Title).map(Some.apply))))
  }

  it should "parse a good header line even if it contains irrelevant fields" in {
    val input = sheet(line("Account", "TransactionDate", "ChargeDate" , "irrelevant", "Title"))
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title)
    val output = new ExcelSheetParser(dict).parse(input)
    assert(output == List(HeaderLine(List(Some(Account), Some(TransactionDate), Some(ChargeDate),None, Some(Title)))))
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
      HeaderLine(List(Account, TransactionDate, ChargeDate, Title).map(Some.apply)),
      LineTooShort(shortLine, 4))
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
      HeaderLine(List(Account, TransactionDate, ChargeDate, Title, PositiveBalance).map(Some.apply)),
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
      HeaderLine(List(Account, TransactionDate, ChargeDate, Title, PositiveBalance).map(Some.apply)),
      ParseSuccess(Transaction("account", someDate, Some(someDate), "title", 1.0, 0.0, None, None).get)
    )
    assert(output == expectedOutput)
  }

  it should "Parse a good transaction line after a good header line with an irrelevant field" in {
    val someDate = new Date()
    val goodLine = line("account", someDate, someDate, "title", "irrelevant", 1.0)
    val input = sheet(
      line("Account", "TransactionDate", "ChargeDate", "Title","irrelevant", "PositiveBalance"),
      goodLine
    )
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title, "PositiveBalance" -> PositiveBalance)
    val output = new ExcelSheetParser(dict).parse(input)
    val expectedOutput = List(
      HeaderLine(List(Some(Account), Some(TransactionDate), Some(ChargeDate), Some(Title), None, Some(PositiveBalance))),
      ParseSuccess(Transaction("account", someDate, Some(someDate), "title", 1.0, 0.0, None, None).get)
    )
    assert(output == expectedOutput)
  }

  def expectedHeaderLine(goodHeader1: ExcelLine, dict: Map[String, FieldType]): HeaderLine = {
    val fieldsList = goodHeader1
      .collect { case StringCell(s) => s }
      .map(dict.get)
    HeaderLine(fieldsList)
  }

  it should "Parse good lines after each new header line" in {
    val goodHeader1 = line("Account", "TransactionDate", "ChargeDate", "Title", "PositiveBalance")
    val date = new Date()
    val goodLine1 = line("account1", date, date, "title1", 1.1)
    val goodHeader2 = line("Account", "TransactionDate", "ChargeDate", "Title", "NegativeBalance")
    val goodLine2 = line("account2", date, date, "title2", 1.1)
    val input = sheet(goodHeader1, goodLine1, goodHeader2, goodLine2)
    val dict = Map("Account" -> Account, "TransactionDate" -> TransactionDate, "ChargeDate" -> ChargeDate, "Title" -> Title, "PositiveBalance" -> PositiveBalance, "NegativeBalance" -> NegativeBalance)

    val output = new ExcelSheetParser(dict).parse(input)
    val expectedOutput = List(
      expectedHeaderLine(goodHeader1, dict),
      ParseSuccess(Transaction("account1", date, Some(date), "title1", 1.1, 0.0, None, None).get),
      expectedHeaderLine(goodHeader2, dict),
      ParseSuccess(Transaction("account2", date, Some(date), "title2", 0.0, 1.1, None, None).get)
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
