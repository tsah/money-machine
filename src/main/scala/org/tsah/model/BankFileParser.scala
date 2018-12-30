package org.tsah.model

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}


case class Transaction(account: String, transactionDate: Date, chargeDate: Date, title: String, positiveBalance: Double, negativeBalance: Double, details: Option[String], assignedType: Option[String]) {
  def toCsvLine: String = s"$account,$transactionDate,$chargeDate,$title,$positiveBalance,$negativeBalance,${details.getOrElse("")},${assignedType.getOrElse("")}"

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case otherTransaction: Transaction =>
        account == otherTransaction.account &&
          transactionDate.toString == otherTransaction.transactionDate.toString &&
          chargeDate.toString == otherTransaction.chargeDate.toString &&
          title == otherTransaction.title
      case _ =>
        false
    }
  }
}

object Transaction {
  val CsvTitleLine = "Account, Transaction Date, Charge Date, Title, Positive Balance, Negative Balance, Details, Assigned Type"

  def saveToCsvFile(transactions: Seq[Transaction], fileName: String): Unit = {
    val pw = new PrintWriter(new File(fileName))
    val toWrite = (Seq(CsvTitleLine) ++ transactions.map(_.toCsvLine)).mkString("\n")
    pw.write(toWrite)
    pw.close()
  }

  val DefaultDict: Map[FieldType, Int] = List(
      Account,
      TransactionDate,
      ChargeDate,
      Title,
      PositiveBalance,
      NegativeBalance,
      TransactionDetails,
      AssignedType
    )
      .zipWithIndex
      .toMap

  val MandatoryFields: Set[FieldType] = Set(Account, TransactionDate, ChargeDate, Title)
}

sealed trait FieldType

case object Account extends FieldType
case object TransactionDate extends FieldType
case object ChargeDate extends FieldType
case object Title extends FieldType
case object PositiveBalance extends FieldType
case object NegativeBalance extends FieldType
case object TransactionDetails extends FieldType
case object AssignedType extends FieldType {
  val Ignore = "ללא"
}


object BankFileParser {

  def parseListFromCreditCardStatement(strings: List[Vector[String]]): List[Transaction] = {
    val interestingLineLength = strings.map(_.size).max
    parseListFromCreditCardStatement(strings, interestingLineLength, None, List())
  }

  def parseListFromBankStatement(strings: List[Vector[String]]): List[Transaction] =
    parseListFromBankStatement(strings, significantLinesStarted = false, List())

  def loadFromSavedList(strings: List[Vector[String]]): List[Transaction] = {
    strings
      .tail
      .flatMap(defaultLineToTransaction)
  }

  def defaultLineToTransaction(line: Vector[String]): Option[Transaction] = lineToTransaction(line, Transaction.DefaultDict)

  def stringToDouble(s: String): Double = {
    if (s.isEmpty) {
      0.0
    } else {
      s.toDouble
    }
  }


  val simpleDateFormat = new SimpleDateFormat("EEE MMM d HH:mm:ss zzz yyyy")
  def tryParseDate(dateStr: String): Option[Date] = {
    Try(simpleDateFormat parse dateStr).toOption
  }

  def lineToTransaction(line: Vector[String], dict: Map[FieldType, Int]): Option[Transaction] = {
    val optionalTransaction = Try {
      val requiredFields = for {
        account <- dict get Account map line.apply
        transactionDate <- dict get TransactionDate map line.apply flatMap tryParseDate
        chargeDate <- dict get ChargeDate map line.apply flatMap tryParseDate
        title <- dict get Title map line.apply
      } yield (account, transactionDate, chargeDate, title)

      requiredFields.flatMap {
        case (account, transactionDate, chargeDate, title) =>
          val optionalDetails = dict get TransactionDetails map line.apply match {
            case Some("") => None
            case other => other
          }
          val optionalAssignedType = dict get AssignedType map line.apply match {
            case Some("") => None
            case other => other
          }
          val optionalPositiveBalance = dict get PositiveBalance map line.apply map stringToDouble
          val optionalNegativeBalance = dict get NegativeBalance map line.apply map stringToDouble
          (optionalPositiveBalance, optionalNegativeBalance) match {
            case (None, None) =>
              None
            case _ =>
              val positiveBalance = optionalPositiveBalance getOrElse 0.0
              val negativeBalance = optionalNegativeBalance getOrElse 0.0
              Some(Transaction(account, transactionDate, chargeDate, title, positiveBalance, negativeBalance, optionalDetails, optionalAssignedType))
          }
      }
    } match {
      case Success(x) =>
        x
      case Failure(ex) =>
        ex.printStackTrace()
        println(s"Failed on line: $line")
        None
    }

    if (optionalTransaction.isEmpty) {
      println(s"Could not parse line: $line")
    }
    optionalTransaction
  }

  def lineToDict(head: Vector[String]): Map[FieldType, Int] =
    head
      .zipWithIndex
      .flatMap { case (fieldName, fieldIndex) =>
        val fieldMapping = FieldMapping.Mapping get fieldName
        if (fieldMapping.isEmpty) {
          println(s"Missing field mapping for: $fieldName")
        }
        fieldMapping map { fieldType => fieldType -> fieldIndex }
      }.toMap


  @tailrec
  private def parseListFromCreditCardStatement(list: List[Vector[String]], interestingLineLength: Int, optionalDict: Option[Map[FieldType, Int]], collectedResults: List[Transaction]): List[Transaction] = {
    list match {
      case Nil => collectedResults
      case head::tail if head.size < interestingLineLength => parseListFromCreditCardStatement(tail, interestingLineLength, None, collectedResults)
      case head::tail =>
        optionalDict match {
          case Some(dict) =>
            parseListFromCreditCardStatement(tail, interestingLineLength, optionalDict, collectedResults ++ List(lineToTransaction(head, dict)).flatten)
          case None =>
            val newDict = lineToDict(head)
            println(s"New dict: $newDict")
            for (mandatoryField <- Transaction.MandatoryFields) {
              if (!newDict.keySet.contains(mandatoryField)) {
                println(s"Mandatory field $mandatoryField missing from dict. Line is: $head")
                throw new RuntimeException("Cannot procceed with mandatory field missing")
              }
            }
            parseListFromCreditCardStatement(tail, interestingLineLength, Some(newDict), collectedResults)
        }
    }
  }

  @tailrec
  private def parseListFromBankStatement(list: List[Vector[String]], significantLinesStarted: Boolean, collectedResults: List[Transaction]): List[Transaction] = {
    (list, significantLinesStarted) match {
      case (Nil, _) =>
        collectedResults
      case (head::tail, false) if head.size < Transaction.MandatoryFields.size =>
        println(s"Skipping line: $head")
        parseListFromBankStatement(tail, significantLinesStarted = false, collectedResults)
      case (_::tail, false) =>
        parseListFromBankStatement(tail, significantLinesStarted = true, collectedResults)
      case (head::tail, true) =>
        val dict = BankDicts.lineLengthToDict(head.size)
        parseListFromBankStatement(tail, significantLinesStarted = true, collectedResults ++ List(lineToTransaction(head, dict)).flatten)
    }
  }

}

object BankDicts {
  val lineLengthToDict: Map[Int, Map[FieldType, Int]] = Map(
    10 -> Map(TransactionDate -> 0, Title -> 1, TransactionDetails -> 2, Account -> 3, NegativeBalance -> 4, PositiveBalance -> 5, ChargeDate -> 7),
    9 -> Map(TransactionDate -> 0, Title -> 1, TransactionDetails -> 2, Account -> 3, NegativeBalance -> 4, PositiveBalance -> 5, ChargeDate -> 7),
    7 -> Map(TransactionDate -> 0, Title -> 1, Account -> 2, NegativeBalance -> 3, PositiveBalance -> 4, ChargeDate -> 6)
  )
}

object FieldMapping {
  val Mapping: Map[String, FieldType] = Map(
    "שם כרטיס" -> Account,
    "חיוב לתאריך" -> ChargeDate,
    "תאריך" -> TransactionDate,
    "שם בית עסק" -> Title,
    "סכום חיוב בש''ח" -> NegativeBalance,
    "זכות" -> PositiveBalance,
    "חובה" -> NegativeBalance,
    "פרטים" -> TransactionDetails,
    "הפעולה" -> Title,
    "תאריך ערך" -> ChargeDate,
    "אסמכתא" -> Account
  )
}