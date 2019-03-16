package org.tsah.parser

import org.tsah.model.Transaction._

object Defaults {
  val Dict: Map[String, FieldType] = Map(
    "תאריך" -> TransactionDate,
    "תאריך ערך" -> ChargeDate,
    "הפעולה" -> Title,
    "פרטים" -> Account,
    "חובה" -> NegativeBalance,
    "זכות" -> PositiveBalance,
    "עבור" -> TransactionDetails
  )
}
