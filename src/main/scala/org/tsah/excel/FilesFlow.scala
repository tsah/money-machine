package org.tsah.excel

import java.io.File

import scala.annotation.tailrec
import scala.io.{Source, StdIn}
import scala.language.postfixOps


object FilesFlow {

  def loadBankLines(filesFolder: String): List[Transaction] = {
    val bankFolder = new File(s"$filesFolder/bank")
    bankFolder.listFiles.flatMap { file =>
      val parsedBankFile = ExcelReader.getStringList(file.getPath)
      BankFileParser.parseListFromBankStatement(parsedBankFile)
    }
      .toList
      .filterNot { transaction =>
        transaction.title == "ישראכרט" ||
        transaction.title == "כרטיסי אשראי ל" ||
        transaction.title == """לאומי קארד בע""""
      }
  }

  def loadCreditCardLines(filesFolder: String): List[Transaction] = {
    val creditCardFolder = new File(s"$filesFolder/credit_cards")
    creditCardFolder.listFiles.flatMap { file =>
      val parsedCreditCardFile = ExcelReader.getStringList(file.getPath)
      BankFileParser.parseListFromCreditCardStatement(parsedCreditCardFile)
    }
      .toList
  }

  def loadMainFileLines(filesFolder: String): List[Transaction] = {
    val mainFile = new File(s"$filesFolder/all_transactions.csv")
    if (mainFile.exists) {
      val savedLines = Source.fromFile(mainFile).getLines().toList.map(_.split(",", -1).toVector)
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

  def mergeLines(mainFileLines: List[Transaction], bankLines: List[Transaction], creditCardLines: List[Transaction]): List[Transaction] = {
    val mergedBankLines = mergeToMainList(mainFileLines, bankLines)
    mergeToMainList(mergedBankLines, creditCardLines)
  }

  def runFlow(filesFolder: String): Unit = {
    val bankLines = loadBankLines(filesFolder)
    val creditCardLines = loadCreditCardLines(filesFolder)
    val mainFileLines = loadMainFileLines(filesFolder)

    val mergedLines  = mergeLines(mainFileLines, bankLines, creditCardLines).sortBy(_.transactionDate)
    Transaction.saveToCsvFile(mergedLines, s"$filesFolder/all_transactions.csv")
  }


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

  def runInteractiveFlow(filesFolder: String): Unit = {
    val bankLines = loadBankLines(filesFolder)
    val creditCardLines = loadCreditCardLines(filesFolder)
    val mainFileLines = loadMainFileLines(filesFolder)

    val mergedLines = mergeLines(mainFileLines, bankLines, creditCardLines)
    val fixedLines = readMissingLinesFromUser(mergedLines)
    Transaction.saveToCsvFile(fixedLines, s"$filesFolder/all_transactions.csv")
  }

}


object InteractiveMain extends App {
  FilesFlow.runInteractiveFlow("files")
}