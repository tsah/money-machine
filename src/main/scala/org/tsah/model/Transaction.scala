package org.tsah.model

import java.io.{File, PrintWriter}
import java.util.Date

import scala.util.Try

case class Transaction(account: String, transactionDate: Date, chargeDate: Date, title: String, positiveBalance: Double, negativeBalance: Double, details: Option[String], assignedType: Option[String]) {
  assert(positiveBalance != 0.0 || negativeBalance != 0.0)
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
  def apply(
             account: String,
             transactionDate: Date,
             chargeDate: Date,
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
  val ManadatoryFieldsAmount: Int = MandatoryFields.size

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