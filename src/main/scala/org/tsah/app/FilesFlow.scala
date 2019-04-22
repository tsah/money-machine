package org.tsah.app

import java.io.{File, PrintWriter}
import java.time.ZoneId

import org.tsah.excel.ExcelModel.ExcelSheet
import org.tsah.excel.{ExcelModel, ExcelReader}
import org.tsah.model.Transaction.AssignedType
import org.tsah.model.{BankFileParser, Transaction}
import org.tsah.parser.ExcelSheetParser.ParseSuccess
import org.tsah.parser.{Defaults, ExcelSheetParser}

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.language.postfixOps


object FilesFlow {

  val BankDir = "bank"
  val CreditCardsDir = "credit-cards"
  val TransactionsFile = "all-transactions.csv"

  def setUpDir(dirName: String): Unit = {
    val dir = new File(dirName)
    if (!dir.exists()) {
      dir.mkdir()
    }
    val bankDir = new File(dir, BankDir)
    if (!bankDir.exists()) {
      bankDir.mkdir()
    }
    val creditCardsDir = new File(dir, CreditCardsDir)
    if (!creditCardsDir.exists) {
      creditCardsDir.mkdir()
    }
  }

  def loadBankLines(filesFolder: String): List[Transaction] = {
//    val bankFolder = new File(s"$filesFolder/$BankDir")
//    bankFolder.listFiles.flatMap { file =>
//        val parsedBankFile = ExcelReader.getStringList(file.getPath)
//        BankFileParser.parseListFromBankStatement(parsedBankFile)
//      }
//      .toList
//      .filterNot { transaction =>
//        transaction.title == "ישראכרט" ||
//        transaction.title == "כרטיסי אשראי ל" ||
//        transaction.title == """לאומי קארד בע""""
//      }
    List()
  }

  def loadCreditCardLines(filesFolder: String): List[Transaction] = {
//    val creditCardFolder = new File(s"$filesFolder/$CreditCardsDir")
//    creditCardFolder.listFiles.flatMap { file =>
//      val parsedCreditCardFile = ExcelReader.getStringList(file.getPath)
//      BankFileParser.parseListFromCreditCardStatement(parsedCreditCardFile)
//      List()
//    }
//      .toList
    List()
  }

  def loadMainFileLines(mainFile: File): List[Transaction] = {
    if (mainFile.exists) {
      val source = Source.fromFile(mainFile)
      val savedLines = source.getLines().toList.map(_.split(",", -1).toVector)
      source.close()
      BankFileParser.loadFromSavedList(savedLines)
    } else {
      List()
    }
  }

  @tailrec
  def mergeToMainList(mainList: List[Transaction], secondaryList: List[Transaction]): List[Transaction]= {
    secondaryList match {
      case Nil =>
        mainList
      case secondaryHead::secondaryTail if mainList.contains(secondaryHead) =>
        mergeToMainList(mainList, secondaryTail)
      case secondaryHead::secondaryTail =>
        mergeToMainList(mainList ++ List(secondaryHead), secondaryTail)
    }
  }

  def mergeLines(mainFileLines: List[Transaction], fromFiles: List[Transaction]): List[Transaction] = {
    mergeToMainList(mainFileLines, fromFiles)
  }

//  def runFlow(filesFolder: String): Unit = {
//    val bankLines = loadBankLines(filesFolder)
//    val creditCardLines = loadCreditCardLines(filesFolder)
//    val mainFileLines = loadMainFileLines(filesFolder)
//
//    val mergedLines  = mergeLines(mainFileLines, bankLines, creditCardLines).sortBy(_.transactionDate)
//    Transaction.saveToCsvFile(mergedLines, s"$filesFolder/$TransactionsFile")
//  }


  def readType(collectedTypes: Set[String]): String = {
    val indexedTypes = collectedTypes.zipWithIndex.map{ case (a,b) => (b,a)}.toMap
    println(s"Available types: $indexedTypes. Select number or enter new type.")
    val line = StdIn.readLine
    readSomeSelection(indexedTypes, line)
  }

  def readSomeSelection(indexedTypes: Map[Int, String], line: String): String = {
    if (line.length == 1 && line.head.isDigit) {
      val index = line.head.toInt
      if (index >= 0 && index < indexedTypes.size) {
        readSelectionWithIndex(indexedTypes, index)
      } else {
        println("Not available for selection")
        readType(indexedTypes.values.toSet)
      }
    } else {
      println(s"Enter to confirm, or new selection")
      val command = StdIn.readLine
      if (command.isEmpty) {
        line
      } else {
        readSomeSelection(indexedTypes, command)
      }
    }
  }

  def readSelectionWithIndex(indexedTypes: Map[Int, String], index: Int): String = {
    val selection = indexedTypes(index)
    println(s"You selected: $selection. Enter to confirm or try new selection")
    val command = StdIn.readLine
    if (command.isEmpty) {
      selection
    } else {
      readSomeSelection(indexedTypes, command)
    }
  }


  def readMissingLinesFromUser(mergedLines: List[Transaction]): List[Transaction] = {
    val existingTypes = mergedLines collect {case Transaction(_,_,_,_,_,_,_,Some(assignedType)) => assignedType} toSet
    val res = mergedLines.foldLeft((List[Transaction](), existingTypes)){(collectedResults, currentTransaction) => collectedResults match {
      case (collectedTransactions: List[Transaction], collectedTypes: Set[String]) =>
        currentTransaction match {
          case Transaction(_,_,_,_,_,_,_,Some(_)) =>
            (collectedTransactions ++ List(currentTransaction), collectedTypes)
          case _ =>
            println(currentTransaction.toCsvLine)
            val userProvidedType = readType(collectedTypes)
            val fixedTransaction = currentTransaction.copy(assignedType = Some(userProvidedType))
            (collectedTransactions ++ List(fixedTransaction), collectedTypes ++ Set(userProvidedType))
        }
      }
    }
      ._1
    res
  }

//  def runInteractiveFlow(filesFolder: String): Unit = {
//    val bankLines = loadBankLines(filesFolder)
//    val creditCardLines = loadCreditCardLines(filesFolder)
//    val mainFileLines = loadMainFileLines(filesFolder)
//
//    val mergedLines = mergeLines(mainFileLines, bankLines, creditCardLines)
//    val fixedLines = readMissingLinesFromUser(mergedLines)
//    Transaction.saveToCsvFile(fixedLines, s"$filesFolder/$TransactionsFile")
//  }

}


object SplitToMonths extends App {
  val filesFolder = new File("files")
  val monthsDir = new File(filesFolder, "months")
  if (!monthsDir.exists()) {
    monthsDir.mkdir()
  }

  val mainFileLines = FilesFlow.loadMainFileLines(new File("files/all_transactions.csv"))
  val completed = mainFileLines.filter(_.assignedType.isDefined).filterNot(_.assignedType.exists(_ == AssignedType.Ignore))
  val splitByMonths = completed.groupBy{transaction =>
    val date = transaction.transactionDate
    val localDate = date.toInstant.atZone(ZoneId.systemDefault()).toLocalDate
    val year = localDate.getYear
    val month = localDate.getMonthValue
    s"$year-$month"
  }
  for ((dateKey, transactions) <- splitByMonths) {
    val targetFile = new File(s"files/months/$dateKey.csv")
    Transaction.saveToCsvFile(transactions, targetFile)
  }

  def sumPaid(transactions: List[Transaction]) = transactions.foldLeft(0.0){(sum: Double, t: Transaction) => sum + t.positiveBalance - t.negativeBalance }

  case class TypeAndSum(assignedType: String, sumPaid: Double) {
    override def toString: String = s"$assignedType : $sumPaid"

    val isPositive: Boolean = sumPaid > 0.0

    val isNegative: Boolean = ! isPositive
  }

  for (monthData <- splitByMonths) {
    val typesAndSums = monthData._2
      .groupBy (_.assignedType)
      .map{
        case (Some(assignedType), transactions) => TypeAndSum(assignedType, sumPaid(transactions))
      }

    val positives = typesAndSums
      .filter(_.isPositive)
      .map(_.toString)
      .mkString("\n")

    val negatives = typesAndSums
      .filter(_.isNegative)
      .map(_.toString)
      .mkString("\n")

    val monthSummary =
      s"""Positive:
         |$positives
         |
         |Negative:
         |$negatives
         |
         |Sum: ${sumPaid(completed)}
         |""".stripMargin

    val monthSummaryFileName = s"files/months/${monthData._1}-summary.txt"
    val pw = new PrintWriter(monthSummaryFileName)
    pw.print(monthSummary)
    pw.close()
  }
}

object NewParserMain extends App {
  val filesDir = new File("files")
  val excelDir = new File(filesDir, "excels")
  val resultFile = new File(filesDir, "all_transactions.csv")
  if (!excelDir.exists()) {excelDir.mkdirs()}
  if (!resultFile.exists()) { resultFile.createNewFile()}
  println("Loading main file")
  val savedTransactions = FilesFlow.loadMainFileLines(resultFile)
  println(s"Loaded ${savedTransactions.size} transactions")
  val parser = new ExcelSheetParser(Defaults.Dict)
  val transactionsFromFiles = excelDir.listFiles()
    .map(ExcelReader.loadExcelSheet)
    .map(parser.parse)
    .flatMap{ parserResult =>
      println(s"File Result: \n ${ExcelSheetParser.detailedPrint(parserResult)}")
      parserResult.collect{ case ParseSuccess(t) => t}
    }
    .toList
  val allTransactions = FilesFlow.mergeLines(savedTransactions, transactionsFromFiles)
  println(s"Writing ${allTransactions.size} transactions to file")
  Transaction.saveToCsvFile(allTransactions, resultFile)
  println("Done")
}