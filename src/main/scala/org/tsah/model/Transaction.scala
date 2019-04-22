package org.tsah.model

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date

import scala.util.Try

case class Transaction(account: String, transactionDate: Date, chargeDate: Option[Date], title: String, positiveBalance: Double, negativeBalance: Double, details: Option[String], assignedType: Option[String]) {
  assert(positiveBalance != 0.0 || negativeBalance != 0.0)
  def toCsvLine: String = s"$account,${Transaction.DateFormat.format(transactionDate)},${chargeDate.map(Transaction.DateFormat.format).getOrElse("")},$title,$positiveBalance,$negativeBalance,${details.getOrElse("")},${assignedType.getOrElse("")}"

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
  def apply(
             account: String,
             transactionDate: Date,
             chargeDate: Option[Date],
             title: String,
             positiveBalance: Double,
             negativeBalance: Double,
             details: Option[String],
             assignedType: Option[String]): Option[Transaction] =
    Try(
      new Transaction(
        account,
        transactionDate,
        chargeDate,
        title,
        positiveBalance,
        negativeBalance,
        details,
        assignedType)
    ).toOption

  val CsvTitleLine = "Account, Transaction Date, Charge Date, Title, Positive Balance, Negative Balance, Details, Assigned Type"

  val DateFormat = new SimpleDateFormat("d/M/yy h:mm a")
  def fromCsvLine(line: Vector[String]): Option[Transaction] = {
    val account = line(0)
    val transactionDate =DateFormat.parse(line(1))
    val chargeDate = if (line(2).isEmpty) {
      None
    } else {
      Some(DateFormat.parse(line(2)))
    }
    val title = line(3)
    val positiveBalance = line(4).toDouble
    val negativeBalance = line(5).toDouble
    val details = if (line(6).isEmpty) None else Some(line(6))
    val assignedType = if (line(7).isEmpty) None else Some(line(7))

    Transaction(account, transactionDate, chargeDate, title, positiveBalance, negativeBalance, details, assignedType)
  }

  def saveToCsvFile(transactions: Seq[Transaction], file: File): Unit = {
    val pw = new PrintWriter(file)
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

  val MandatoryFields: Set[FieldType] = Set(TransactionDate, Title)
  val MandatoryFieldsAmount: Int = MandatoryFields.size+1

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
}