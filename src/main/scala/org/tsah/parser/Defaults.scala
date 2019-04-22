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
    "עבור" -> TransactionDetails,
    "תיאור הפעולה" -> Title,
    "תאריך עסקה" -> TransactionDate,
    "תאריך חיוב" -> ChargeDate,
    "שם בית העסק" -> Title,
    "סכום חיוב ₪" -> NegativeBalance,
    "תאריך רכישה" -> TransactionDate,
    "שם בית עסק" -> Title,
    "סכום לחיוב" -> NegativeBalance,
    "תאריך העסקה" -> TransactionDate,
    "שם בית העסק" -> Title,
    "סכום החיוב" -> NegativeBalance,
    "סכום העסקה" -> NegativeBalance

  )
}
