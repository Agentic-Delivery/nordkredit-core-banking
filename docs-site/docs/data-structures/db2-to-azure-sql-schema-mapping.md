---
title: Db2-to-Azure SQL Schema Mapping
sidebar_position: 20
---

# Db2-to-Azure SQL Schema Mapping

Comprehensive field-level mapping from IBM Db2 for z/OS (COBOL VSAM/copybook structures) to Azure SQL Database for the NordKredit core banking migration.

## Overview

| Attribute | Source (Mainframe) | Target (Azure) |
|---|---|---|
| **Database** | IBM Db2 for z/OS | Azure SQL Database |
| **Character encoding** | EBCDIC (IBM Code Page 1143 — Swedish/Finnish) | Unicode (UTF-8 / nvarchar) |
| **File system** | VSAM (KSDS, sequential) | Relational tables |
| **ORM** | N/A | Entity Framework Core 8 |
| **Migration** | `NordKredit.Infrastructure.Migrations.InitialCreate` | EF Core code-first migration |
| **Tables** | 9 | 9 |

### Converter utility

All EBCDIC-to-Unicode conversions use `NordKredit.Infrastructure.DataMigration.EbcdicConverter`:

| Conversion | Method | Notes |
|---|---|---|
| Text (`PIC X`) | `ConvertToUnicode()` | IBM Code Page 1143 (Swedish/Finnish) |
| Unsigned numeric (`PIC 9`) | `ConvertZonedDecimal(scale: 0)` | Zoned decimal, digit in low nibble |
| Signed decimal (`PIC S9 V99`) | `ConvertZonedDecimal(scale: 2)` | Sign in high nibble of last byte |
| Packed decimal (`COMP-3`) | `ConvertPackedDecimal(scale)` | BCD nibble pairs, sign in last nibble |
| Timestamp (`PIC X(26)`) | `ConvertToUnicode()` then `DateTime.ParseExact()` | Two-step: decode text, then parse |

---

## Table 1: Accounts

| Attribute | Value |
|---|---|
| **Copybook** | CVACT01Y.cpy |
| **COBOL record** | `ACCOUNT-RECORD` (300 bytes) |
| **VSAM file** | `ACCTFILE` (KSDS, key = `ACCT-ID`) |
| **Azure SQL table** | `Accounts` |
| **.NET class** | `NordKredit.Domain.Transactions.Account` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `ACCT-ID` | `PIC 9(11)` | 0 | 11 | `Id` | `nvarchar(11)` | `Id` | `string` | `ConvertZonedDecimal(scale: 0)` → string | Primary key. Stored as string to preserve leading zeros. |
| 2 | `ACCT-ACTIVE-STATUS` | `PIC X(01)` | 11 | 1 | `ActiveStatus` | `nvarchar(1)` | `ActiveStatus` | `string` | `ConvertToUnicode()` | `'A'` = active, `'D'` = dormant |
| 3 | `ACCT-CURR-BAL` | `PIC S9(10)V99` | 12 | 12 | `CurrentBalance` | `decimal(12,2)` | `CurrentBalance` | `decimal` | `ConvertZonedDecimal(scale: 2)` | Signed, 10 integer + 2 decimal digits |
| 4 | `ACCT-CREDIT-LIMIT` | `PIC S9(10)V99` | 24 | 12 | `CreditLimit` | `decimal(12,2)` | `CreditLimit` | `decimal` | `ConvertZonedDecimal(scale: 2)` | |
| 5 | `ACCT-CASH-CREDIT-LIMIT` | `PIC S9(10)V99` | 36 | 12 | `CashCreditLimit` | `decimal(12,2)` | `CashCreditLimit` | `decimal` | `ConvertZonedDecimal(scale: 2)` | |
| 6 | `ACCT-CURR-CYC-CREDIT` | `PIC S9(10)V99` | 48 | 12 | `CurrentCycleCredit` | `decimal(12,2)` | `CurrentCycleCredit` | `decimal` | `ConvertZonedDecimal(scale: 2)` | |
| 7 | `ACCT-CURR-CYC-DEBIT` | `PIC S9(10)V99` | 60 | 12 | `CurrentCycleDebit` | `decimal(12,2)` | `CurrentCycleDebit` | `decimal` | `ConvertZonedDecimal(scale: 2)` | |
| 8 | `ACCT-EXPIRAION-DATE` | `PIC X(10)` | 72 | 10 | `ExpirationDate` | `datetime2` | `ExpirationDate` | `DateTime?` | `ConvertToUnicode()` then parse | Nullable — `null` means never expires. COBOL field has typo (`EXPIRAION`). |
| 9 | `FILLER` | `PIC X(218)` | 82 | 218 | — | — | — | — | — | Reserved / padding. Not migrated. |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `ACCTFILE` KSDS primary key (`ACCT-ID`) | `PK_Accounts` on `Id` |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| FFFS 2014:5 Ch.4 &sect;3 | Credit limit enforcement for risk management |
| PSD2 Art. 97 | Account validation during SCA flows |
| GDPR Art. 5(1)(d) | Accuracy of balance data during migration |

---

## Table 2: Cards

| Attribute | Value |
|---|---|
| **Copybook** | CVACT02Y.cpy |
| **COBOL record** | `CARD-RECORD` (150 bytes) |
| **VSAM file** | `CARDDAT` (KSDS, key = `CARD-NUM`), `CARDAIX` (alternate index on `CARD-ACCT-ID`) |
| **Azure SQL table** | `Cards` |
| **.NET class** | `NordKredit.Domain.CardManagement.Card` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `CARD-NUM` | `PIC X(16)` | 0 | 16 | `CardNumber` | `nvarchar(16)` | `CardNumber` | `string` | `ConvertToUnicode()` | Primary key |
| 2 | `CARD-ACCT-ID` | `PIC 9(11)` | 16 | 11 | `AccountId` | `nvarchar(11)` | `AccountId` | `string` | `ConvertZonedDecimal(scale: 0)` → string | FK to Accounts. String to preserve leading zeros. |
| 3 | `CARD-CVV-CD` | `PIC 9(03)` | 27 | 3 | `CvvCode` | `nvarchar(3)` | `CvvCode` | `string` | `ConvertZonedDecimal(scale: 0)` → string | PCI-DSS review required for storage. |
| 4 | `CARD-EMBOSSED-NAME` | `PIC X(50)` | 30 | 50 | `EmbossedName` | `nvarchar(50)` | `EmbossedName` | `string` | `ConvertToUnicode()` | Swedish chars (Å, Ä, Ö) — requires Code Page 1143. |
| 5 | `CARD-EXPIRAION-DATE` | `PIC X(10)` | 80 | 10 | `ExpirationDate` | `date` | `ExpirationDate` | `DateOnly` | `ConvertToUnicode()` then parse | COBOL field has typo. .NET uses corrected spelling. |
| 6 | `CARD-ACTIVE-STATUS` | `PIC X(01)` | 90 | 1 | `ActiveStatus` | `nvarchar(1)` | `ActiveStatus` | `char` | `ConvertToUnicode()` | `'Y'` = active, `'N'` = inactive |
| 7 | `FILLER` | `PIC X(59)` | 91 | 59 | — | — | — | — | — | Padding. Not migrated. |
| — | *(no COBOL equivalent)* | — | — | — | `RowVersion` | `rowversion` | `RowVersion` | `byte[]` | — | Optimistic concurrency. Replaces COBOL field-by-field comparison. |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `CARDDAT` KSDS primary key (`CARD-NUM`) | `PK_Cards` on `CardNumber` |
| `CARDAIX` alternate index (`CARD-ACCT-ID`) | `IX_Cards_AccountId` on `AccountId` |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| GDPR Art. 5(1)(c) | Data minimization — only store necessary card fields |
| GDPR Art. 5(1)(d) | Accuracy — cardholder name must match embossed card |
| PSD2 Art. 97 | Strong Customer Authentication for card operations |
| PCI-DSS | CVV storage review required post-migration |

---

## Table 3: CardCrossReferences

| Attribute | Value |
|---|---|
| **Copybook** | CVACT03Y.cpy |
| **COBOL record** | `CARD-XREF-RECORD` (50 bytes) |
| **VSAM file** | `CXREF` (KSDS, key = `XREF-CARD-NUM`) |
| **Azure SQL table** | `CardCrossReferences` |
| **.NET class** | `NordKredit.Domain.Transactions.CardCrossReference` / `NordKredit.Domain.CardManagement.CardCrossReference` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `XREF-CARD-NUM` | `PIC X(16)` | 0 | 16 | `CardNumber` | `nvarchar(16)` | `CardNumber` | `string` | `ConvertToUnicode()` | Primary key |
| 2 | `XREF-CUST-ID` | `PIC 9(09)` | 16 | 9 | `CustomerId` | `int` | `CustomerId` | `int` | `ConvertZonedDecimal(scale: 0)` | Numeric — no meaningful leading zeros |
| 3 | `XREF-ACCT-ID` | `PIC 9(11)` | 25 | 11 | `AccountId` | `nvarchar(11)` | `AccountId` | `string` | `ConvertZonedDecimal(scale: 0)` → string | Preserves leading zeros |
| 4 | `FILLER` | `PIC X(14)` | 36 | 14 | — | — | — | — | — | Padding. Not migrated. |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `CXREF` KSDS primary key (`XREF-CARD-NUM`) | `PK_CardCrossReferences` on `CardNumber` |
| *(no VSAM equivalent)* | `IX_CardCrossReferences_AccountId` on `AccountId` |
| *(no VSAM equivalent)* | `IX_CardCrossReferences_CustomerId` on `CustomerId` |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| GDPR Art. 5(1)(c) | Data minimization — only card/customer/account linkage |
| AML/KYC | Card-to-customer linkage for suspicious activity monitoring |

---

## Table 4: Transactions

| Attribute | Value |
|---|---|
| **Copybook** | CVTRA05Y.cpy |
| **COBOL record** | `TRAN-RECORD` (350 bytes) |
| **VSAM file** | `TRANSACT` (KSDS, key = `TRAN-ID`) |
| **Azure SQL table** | `Transactions` |
| **.NET class** | `NordKredit.Domain.Transactions.Transaction` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `TRAN-ID` | `PIC X(16)` | 0 | 16 | `Id` | `nvarchar(16)` | `Id` | `string` | `ConvertToUnicode()` | Primary key |
| 2 | `TRAN-TYPE-CD` | `PIC X(02)` | 16 | 2 | `TypeCode` | `nvarchar(2)` | `TypeCode` | `string` | `ConvertToUnicode()` | FK to TransactionTypes (`"DB"`, `"CR"`) |
| 3 | `TRAN-CAT-CD` | `PIC 9(04)` | 18 | 4 | `CategoryCode` | `int` | `CategoryCode` | `int` | `ConvertZonedDecimal(scale: 0)` | FK to TransactionCategories (1001–9999) |
| 4 | `TRAN-SOURCE` | `PIC X(10)` | 22 | 10 | `Source` | `nvarchar(10)` | `Source` | `string` | `ConvertToUnicode()` | `"ONLINE"`, `"BATCH"` |
| 5 | `TRAN-DESC` | `PIC X(100)` | 32 | 100 | `Description` | `nvarchar(100)` | `Description` | `string` | `ConvertToUnicode()` | May contain Swedish characters |
| 6 | `TRAN-AMT` | `PIC S9(09)V99` | 132 | 11 | `Amount` | `decimal(11,2)` | `Amount` | `decimal` | `ConvertZonedDecimal(scale: 2)` | 9 integer + 2 decimal digits |
| 7 | `TRAN-MERCHANT-ID` | `PIC 9(09)` | 143 | 9 | `MerchantId` | `int` | `MerchantId` | `int` | `ConvertZonedDecimal(scale: 0)` | |
| 8 | `TRAN-MERCHANT-NAME` | `PIC X(50)` | 152 | 50 | `MerchantName` | `nvarchar(50)` | `MerchantName` | `string` | `ConvertToUnicode()` | May contain Swedish characters |
| 9 | `TRAN-MERCHANT-CITY` | `PIC X(50)` | 202 | 50 | `MerchantCity` | `nvarchar(50)` | `MerchantCity` | `string` | `ConvertToUnicode()` | May contain Swedish characters |
| 10 | `TRAN-MERCHANT-ZIP` | `PIC X(10)` | 252 | 10 | `MerchantZip` | `nvarchar(10)` | `MerchantZip` | `string` | `ConvertToUnicode()` | |
| 11 | `TRAN-CARD-NUM` | `PIC X(16)` | 262 | 16 | `CardNumber` | `nvarchar(16)` | `CardNumber` | `string` | `ConvertToUnicode()` | FK to Cards |
| 12 | `TRAN-ORIG-TS` | `PIC X(26)` | 278 | 26 | `OriginationTimestamp` | `datetime2` | `OriginationTimestamp` | `DateTime` | `ConvertToUnicode()` then `DateTime.ParseExact()` | COBOL format: `YYYY-MM-DD-HH.MM.SS.FFFFFF` |
| 13 | `TRAN-PROC-TS` | `PIC X(26)` | 304 | 26 | `ProcessingTimestamp` | `datetime2` | `ProcessingTimestamp` | `DateTime` | `ConvertToUnicode()` then `DateTime.ParseExact()` | |
| 14 | `FILLER` | `PIC X(20)` | 330 | 20 | — | — | — | — | — | Padding. Not migrated. |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `TRANSACT` KSDS primary key (`TRAN-ID`) | `PK_Transactions` on `Id` |
| *(VSAM alternate index on TRAN-CARD-NUM)* | `IX_Transactions_CardNumber` on `CardNumber` |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| PSD2 Art. 94 | Record keeping — complete transaction audit trail |
| FSA FFFS 2014:5 Ch.7 | Financial reporting — accurate transaction records |
| AML/KYC | Transaction monitoring — merchant and amount data |
| GDPR Art. 5(1)(d) | Data accuracy during migration |

---

## Table 5: DailyTransactions

| Attribute | Value |
|---|---|
| **Copybook** | CVTRA06Y.cpy |
| **COBOL record** | `DALYTRAN-RECORD` (350 bytes) |
| **VSAM file** | `DALYTRAN` (sequential daily input file) |
| **Azure SQL table** | `DailyTransactions` |
| **.NET class** | `NordKredit.Domain.Transactions.DailyTransaction` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `DALYTRAN-ID` | `PIC X(16)` | 0 | 16 | `Id` | `nvarchar(16)` | `Id` | `string` | `ConvertToUnicode()` | Primary key |
| 2 | `DALYTRAN-TYPE-CD` | `PIC X(02)` | 16 | 2 | `TypeCode` | `nvarchar(2)` | `TypeCode` | `string` | `ConvertToUnicode()` | |
| 3 | `DALYTRAN-CAT-CD` | `PIC 9(04)` | 18 | 4 | `CategoryCode` | `int` | `CategoryCode` | `int` | `ConvertZonedDecimal(scale: 0)` | |
| 4 | `DALYTRAN-SOURCE` | `PIC X(10)` | 22 | 10 | `Source` | `nvarchar(10)` | `Source` | `string` | `ConvertToUnicode()` | |
| 5 | `DALYTRAN-DESC` | `PIC X(100)` | 32 | 100 | `Description` | `nvarchar(100)` | `Description` | `string` | `ConvertToUnicode()` | May contain Swedish characters |
| 6 | `DALYTRAN-AMT` | `PIC S9(09)V99` | 132 | 11 | `Amount` | `decimal(11,2)` | `Amount` | `decimal` | `ConvertZonedDecimal(scale: 2)` | |
| 7 | `DALYTRAN-MERCHANT-ID` | `PIC 9(09)` | 143 | 9 | `MerchantId` | `int` | `MerchantId` | `int` | `ConvertZonedDecimal(scale: 0)` | |
| 8 | `DALYTRAN-MERCHANT-NAME` | `PIC X(50)` | 152 | 50 | `MerchantName` | `nvarchar(50)` | `MerchantName` | `string` | `ConvertToUnicode()` | May contain Swedish characters |
| 9 | `DALYTRAN-MERCHANT-CITY` | `PIC X(50)` | 202 | 50 | `MerchantCity` | `nvarchar(50)` | `MerchantCity` | `string` | `ConvertToUnicode()` | May contain Swedish characters |
| 10 | `DALYTRAN-MERCHANT-ZIP` | `PIC X(10)` | 252 | 10 | `MerchantZip` | `nvarchar(10)` | `MerchantZip` | `string` | `ConvertToUnicode()` | |
| 11 | `DALYTRAN-CARD-NUM` | `PIC X(16)` | 262 | 16 | `CardNumber` | `nvarchar(16)` | `CardNumber` | `string` | `ConvertToUnicode()` | |
| 12 | `DALYTRAN-ORIG-TS` | `PIC X(26)` | 278 | 26 | `OriginationTimestamp` | `datetime2` | `OriginationTimestamp` | `DateTime` | `ConvertToUnicode()` then `DateTime.ParseExact()` | |
| 13 | `DALYTRAN-PROC-TS` | `PIC X(26)` | 304 | 26 | `ProcessingTimestamp` | `datetime2` | `ProcessingTimestamp` | `DateTime` | `ConvertToUnicode()` then `DateTime.ParseExact()` | |
| 14 | `FILLER` | `PIC X(20)` | 330 | 20 | — | — | — | — | — | Padding. Not migrated. |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `DALYTRAN` sequential file (no VSAM key) | `PK_DailyTransactions` on `Id` |
| *(no VSAM equivalent)* | `IX_DailyTransactions_CardNumber` on `CardNumber` |

### Design note

The DailyTransactions table mirrors the Transactions table exactly. In COBOL, both copybooks (CVTRA05Y and CVTRA06Y) share the same field layout with different prefixes (`TRAN-` vs `DALYTRAN-`). Daily transactions are input records processed by the nightly batch cycle: verified by CBTRN01C, then posted as Transactions by CBTRN02C (or rejected to DailyRejects). In .NET, this batch process maps to an Azure Function timer trigger.

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| PSD2 Art. 97 | SCA validation during transaction verification |
| FFFS 2014:5 Ch.4 &sect;3 | Credit risk checks before posting |
| EBA Guidelines | Creditworthiness assessment during posting |
| GDPR Art. 5(1)(d) | Data accuracy of incoming transaction data |

---

## Table 6: DailyRejects

| Attribute | Value |
|---|---|
| **Copybook** | *(derived from CBTRN02C.cbl:370–422 — no dedicated copybook)* |
| **COBOL record** | *(composed from DALYTRAN fields + reject metadata)* |
| **VSAM file** | *(reject output file — sequential)* |
| **Azure SQL table** | `DailyRejects` |
| **.NET class** | `NordKredit.Domain.Transactions.DailyReject` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `DALYTRAN-ID` | `PIC X(16)` | — | 16 | `TransactionId` | `nvarchar(16)` | `TransactionId` | `string` | `ConvertToUnicode()` | Composite PK part 1. From rejected daily transaction. |
| 2 | `DALYTRAN-CARD-NUM` | `PIC X(16)` | — | 16 | `CardNumber` | `nvarchar(16)` | `CardNumber` | `string` | `ConvertToUnicode()` | |
| 3 | `XREF-ACCT-ID` | `PIC 9(11)` | — | 11 | `AccountId` | `nvarchar(11)` | `AccountId` | `string` | `ConvertZonedDecimal(scale: 0)` → string | From cross-reference lookup |
| 4 | *(reject code)* | *(internal)* | — | — | `RejectCode` | `int` | `RejectCode` | `int` | — | Composite PK part 2. Codes: 100=invalid card, 101=no account, 102=overlimit, 103=expired. |
| 5 | *(reject reason)* | *(internal)* | — | — | `RejectReason` | `nvarchar(100)` | `RejectReason` | `string` | — | Human-readable rejection description |
| 6 | `DALYTRAN-AMT` | `PIC S9(09)V99` | — | 11 | `TransactionAmount` | `decimal(11,2)` | `TransactionAmount` | `decimal` | `ConvertZonedDecimal(scale: 2)` | Amount at time of rejection |
| 7 | *(timestamp)* | *(internal)* | — | — | `RejectedAt` | `datetime2` | `RejectedAt` | `DateTime` | — | When the rejection was recorded |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| Sequential reject output file | `PK_DailyRejects` on (`TransactionId`, `RejectCode`) |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| PSD2 Art. 97 | SCA failure audit trail |
| FFFS 2014:5 Ch.4 &sect;3 | Credit risk rejection records |
| EBA Guidelines | Creditworthiness rejection audit |

---

## Table 7: TransactionCategoryBalances

| Attribute | Value |
|---|---|
| **Copybook** | CVTRA01Y.cpy |
| **COBOL record** | `TRAN-CAT-BAL-RECORD` (50 bytes) |
| **VSAM file** | `TCATBALF` (KSDS, composite key = `TRAN-CAT-KEY`) |
| **Azure SQL table** | `TransactionCategoryBalances` |
| **.NET class** | `NordKredit.Domain.Transactions.TransactionCategoryBalance` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `TRANCAT-ACCT-ID` | `PIC 9(11)` | 0 | 11 | `AccountId` | `nvarchar(11)` | `AccountId` | `string` | `ConvertZonedDecimal(scale: 0)` → string | Composite PK part 1 |
| 2 | `TRANCAT-TYPE-CD` | `PIC X(02)` | 11 | 2 | `TypeCode` | `nvarchar(2)` | `TypeCode` | `string` | `ConvertToUnicode()` | Composite PK part 2 |
| 3 | `TRANCAT-CD` | `PIC 9(04)` | 13 | 4 | `CategoryCode` | `int` | `CategoryCode` | `int` | `ConvertZonedDecimal(scale: 0)` | Composite PK part 3 |
| 4 | `TRAN-CAT-BAL` | `PIC S9(09)V99` | 17 | 11 | `Balance` | `decimal(11,2)` | `Balance` | `decimal` | `ConvertZonedDecimal(scale: 2)` | 9 integer + 2 decimal digits |
| 5 | `FILLER` | `PIC X(22)` | 28 | 22 | — | — | — | — | — | Padding. Not migrated. |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `TCATBALF` KSDS composite key (`TRAN-CAT-KEY` = ACCT-ID + TYPE-CD + CAT-CD, 17 bytes) | `PK_TransactionCategoryBalances` on (`AccountId`, `TypeCode`, `CategoryCode`) |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| FSA FFFS 2014:5 Ch.7 | Financial reporting — accurate category-level balances |
| PSD2 Art. 94 | Record keeping — transaction classification |

---

## Table 8: TransactionTypes

| Attribute | Value |
|---|---|
| **Copybook** | CVTRA03Y.cpy |
| **COBOL record** | `TRAN-TYPE-RECORD` (~52 bytes) |
| **VSAM file** | `TRANTYPE` (KSDS, key = `TRAN-TYPE`) |
| **Azure SQL table** | `TransactionTypes` |
| **.NET class** | `NordKredit.Domain.Transactions.TransactionType` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `TRAN-TYPE` | `PIC X(02)` | 0 | 2 | `TypeCode` | `nvarchar(2)` | `TypeCode` | `string` | `ConvertToUnicode()` | Primary key |
| 2 | `TRAN-TYPE-DESC` | `PIC X(50)` | 2 | 50 | `Description` | `nvarchar(50)` | `Description` | `string` | `ConvertToUnicode()` | May contain Swedish characters |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `TRANTYPE` KSDS primary key (`TRAN-TYPE`) | `PK_TransactionTypes` on `TypeCode` |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| FSA FFFS 2014:5 Ch.7 | Financial reporting — clear transaction type classification |
| PSD2 Art. 94 | Accessibility — human-readable transaction information |

---

## Table 9: TransactionCategories

| Attribute | Value |
|---|---|
| **Copybook** | CVTRA04Y.cpy |
| **COBOL record** | `TRAN-CAT-RECORD` (~56 bytes) |
| **VSAM file** | `TRANCATG` (KSDS, composite key = `TRAN-TYPE` + `TRAN-CAT-CD`) |
| **Azure SQL table** | `TransactionCategories` |
| **.NET class** | `NordKredit.Domain.Transactions.TransactionCategory` |

### Field mapping

| # | COBOL Field | PIC Clause | COBOL Offset | COBOL Length | SQL Column | SQL Type | C# Property | C# Type | EBCDIC Conversion | Notes |
|---|---|---|---|---|---|---|---|---|---|---|
| 1 | `TRAN-TYPE` | `PIC X(02)` | 0 | 2 | `TypeCode` | `nvarchar(2)` | `TypeCode` | `string` | `ConvertToUnicode()` | Composite PK part 1 |
| 2 | `TRAN-CAT-CD` | `PIC 9(04)` | 2 | 4 | `CategoryCode` | `int` | `CategoryCode` | `int` | `ConvertZonedDecimal(scale: 0)` | Composite PK part 2. Pure numeric — no meaningful leading zeros. |
| 3 | `TRAN-CAT-DESC` | `PIC X(50)` | 6 | 50 | `Description` | `nvarchar(50)` | `Description` | `string` | `ConvertToUnicode()` | May contain Swedish characters |

### Key and index mapping

| COBOL / VSAM | Azure SQL |
|---|---|
| `TRANCATG` KSDS composite key (`TRAN-TYPE` + `TRAN-CAT-CD`, 6 bytes) | `PK_TransactionCategories` on (`TypeCode`, `CategoryCode`) |

### Regulatory traceability

| Regulation | Requirement |
|---|---|
| FSA FFFS 2014:5 Ch.7 | Financial reporting — granular transaction classification |
| PSD2 Art. 94 | Accessibility — detailed transaction categorization |

---

## VSAM-to-SQL Index Summary

Complete mapping of all VSAM key structures and alternate indexes to Azure SQL equivalents.

| VSAM File | VSAM Key Type | COBOL Key Fields | Azure SQL Table | SQL Index Name | SQL Columns | SQL Key Type |
|---|---|---|---|---|---|---|
| `ACCTFILE` | KSDS primary | `ACCT-ID` | `Accounts` | `PK_Accounts` | `Id` | Primary key |
| `CARDDAT` | KSDS primary | `CARD-NUM` | `Cards` | `PK_Cards` | `CardNumber` | Primary key |
| `CARDDAT` | Alternate index (`CARDAIX`) | `CARD-ACCT-ID` | `Cards` | `IX_Cards_AccountId` | `AccountId` | Non-unique index |
| `CXREF` | KSDS primary | `XREF-CARD-NUM` | `CardCrossReferences` | `PK_CardCrossReferences` | `CardNumber` | Primary key |
| *(no VSAM)* | — | — | `CardCrossReferences` | `IX_CardCrossReferences_AccountId` | `AccountId` | Non-unique index |
| *(no VSAM)* | — | — | `CardCrossReferences` | `IX_CardCrossReferences_CustomerId` | `CustomerId` | Non-unique index |
| `TRANSACT` | KSDS primary | `TRAN-ID` | `Transactions` | `PK_Transactions` | `Id` | Primary key |
| *(VSAM AIX)* | Alternate index | `TRAN-CARD-NUM` | `Transactions` | `IX_Transactions_CardNumber` | `CardNumber` | Non-unique index |
| `DALYTRAN` | Sequential (no key) | — | `DailyTransactions` | `PK_DailyTransactions` | `Id` | Primary key |
| *(no VSAM)* | — | — | `DailyTransactions` | `IX_DailyTransactions_CardNumber` | `CardNumber` | Non-unique index |
| *(sequential)* | — | — | `DailyRejects` | `PK_DailyRejects` | `TransactionId`, `RejectCode` | Composite primary key |
| `TCATBALF` | KSDS composite | `TRAN-CAT-KEY` | `TransactionCategoryBalances` | `PK_TransactionCategoryBalances` | `AccountId`, `TypeCode`, `CategoryCode` | Composite primary key |
| `TRANTYPE` | KSDS primary | `TRAN-TYPE` | `TransactionTypes` | `PK_TransactionTypes` | `TypeCode` | Primary key |
| `TRANCATG` | KSDS composite | `TRAN-TYPE` + `TRAN-CAT-CD` | `TransactionCategories` | `PK_TransactionCategories` | `TypeCode`, `CategoryCode` | Composite primary key |

---

## COBOL PIC-to-SQL Type Reference

Summary of all type conversions used across the 9 tables.

| COBOL PIC Clause | Meaning | Azure SQL Type | C# Type | EBCDIC Conversion Method | Fields Using This Pattern |
|---|---|---|---|---|---|
| `PIC X(n)` | Alphanumeric, n chars | `nvarchar(n)` | `string` | `ConvertToUnicode()` | CardNumber, EmbossedName, Description, Source, MerchantName, MerchantCity, MerchantZip, ActiveStatus, TypeCode |
| `PIC 9(n)` | Unsigned numeric, n digits | `int` | `int` | `ConvertZonedDecimal(scale: 0)` | MerchantId, CategoryCode, CustomerId, RejectCode |
| `PIC 9(n)` → string | Unsigned numeric preserving leading zeros | `nvarchar(n)` | `string` | `ConvertZonedDecimal(scale: 0)` → `.ToString().PadLeft(n, '0')` | AccountId (11 digits), CvvCode (3 digits) |
| `PIC S9(n)V99` | Signed decimal, 2 implied places | `decimal(n+2,2)` | `decimal` | `ConvertZonedDecimal(scale: 2)` | Amount, Balance, CurrentBalance, CreditLimit, CashCreditLimit, CurrentCycleCredit, CurrentCycleDebit |
| `PIC X(10)` (date) | Date as string | `date` or `datetime2` | `DateOnly` or `DateTime?` | `ConvertToUnicode()` then parse | ExpirationDate (Cards), ExpirationDate (Accounts) |
| `PIC X(26)` (timestamp) | Timestamp as string | `datetime2` | `DateTime` | `ConvertToUnicode()` then `DateTime.ParseExact()` | OriginationTimestamp, ProcessingTimestamp |
| `FILLER` | Padding / reserved bytes | *(not migrated)* | — | — | All records |

---

## EBCDIC-to-Unicode Conversion Requirements by Table

Fields that require special attention during data migration due to Swedish character support (Å, Ä, Ö) or signed numeric encoding.

| Table | Field | Conversion | Special Attention |
|---|---|---|---|
| **Cards** | `EmbossedName` | `ConvertToUnicode()` | Swedish characters (Å, Ä, Ö) — must use Code Page 1143 |
| **Transactions** | `Description` | `ConvertToUnicode()` | Swedish characters possible |
| **Transactions** | `MerchantName` | `ConvertToUnicode()` | Swedish characters possible |
| **Transactions** | `MerchantCity` | `ConvertToUnicode()` | Swedish city names (e.g., Malmö, Göteborg, Linköping) |
| **Transactions** | `OriginationTimestamp` | `ConvertToUnicode()` + parse | Two-step: EBCDIC → string → `DateTime`. Format: `YYYY-MM-DD-HH.MM.SS.FFFFFF` |
| **Transactions** | `ProcessingTimestamp` | `ConvertToUnicode()` + parse | Same two-step conversion |
| **DailyTransactions** | *(same as Transactions)* | *(same)* | Identical layout |
| **TransactionTypes** | `Description` | `ConvertToUnicode()` | May contain Swedish in localized deployments |
| **TransactionCategories** | `Description` | `ConvertToUnicode()` | May contain Swedish in localized deployments |
| **All tables** | Signed decimal fields | `ConvertZonedDecimal(scale: 2)` | Sign encoded in zone nibble of last byte — verify positive/negative sign (0xC/0xF = positive, 0xD = negative) |

---

## Data Validation Checklist (Post-Migration)

Use this checklist to verify data integrity after each migration batch and during parallel-run comparison.

### Record counts

- [ ] Row count per table matches mainframe record count
- [ ] `Accounts` count matches `ACCTFILE` VSAM record count
- [ ] `Cards` count matches `CARDDAT` VSAM record count
- [ ] `CardCrossReferences` count matches `CXREF` VSAM record count
- [ ] `Transactions` count matches `TRANSACT` VSAM record count
- [ ] `TransactionCategoryBalances` count matches `TCATBALF` VSAM record count
- [ ] `TransactionTypes` count matches `TRANTYPE` VSAM record count
- [ ] `TransactionCategories` count matches `TRANCATG` VSAM record count

### Primary key integrity

- [ ] No duplicate primary keys in any table
- [ ] All composite keys (`DailyRejects`, `TransactionCategoryBalances`, `TransactionCategories`) have no duplicate combinations
- [ ] No null values in primary key columns

### Referential integrity

- [ ] Every `Cards.AccountId` has a matching `Accounts.Id`
- [ ] Every `CardCrossReferences.AccountId` has a matching `Accounts.Id`
- [ ] Every `Transactions.CardNumber` has a matching `Cards.CardNumber`
- [ ] Every `Transactions.TypeCode` has a matching `TransactionTypes.TypeCode`
- [ ] Every `DailyTransactions.CardNumber` has a matching `Cards.CardNumber`
- [ ] Every `TransactionCategoryBalances.TypeCode` has a matching `TransactionTypes.TypeCode`

### Numeric precision

- [ ] All `decimal(12,2)` balance fields (Accounts) round-trip without precision loss vs. mainframe values
- [ ] All `decimal(11,2)` amount fields (Transactions, DailyTransactions, DailyRejects, TransactionCategoryBalances) match mainframe exactly
- [ ] No truncation of signed decimal values — verify negative amounts convert correctly (zone nibble 0xD)
- [ ] Verify boundary values: maximum `PIC S9(10)V99` = &plusmn;9,999,999,999.99

### Character encoding

- [ ] Swedish characters (Å, Ä, Ö, å, ä, ö) display correctly in `EmbossedName`, `Description`, `MerchantName`, `MerchantCity` fields
- [ ] No EBCDIC residue (e.g., `0xC1` appearing instead of `'A'`)
- [ ] `FILLER` bytes are not carried into SQL columns
- [ ] Leading/trailing spaces from fixed-length COBOL fields are trimmed appropriately

### Date and timestamp conversion

- [ ] `Cards.ExpirationDate` (`DateOnly`) parsed correctly from COBOL `PIC X(10)` string
- [ ] `Accounts.ExpirationDate` (`DateTime?`) handles null correctly (null = never expires)
- [ ] Transaction timestamps (`datetime2`) parsed from COBOL `YYYY-MM-DD-HH.MM.SS.FFFFFF` format
- [ ] No timezone drift — mainframe timestamps are local (CET/CEST); confirm Azure SQL stores consistently

### Status and code fields

- [ ] `Accounts.ActiveStatus` contains only `'A'` or `'D'`
- [ ] `Cards.ActiveStatus` contains only `'Y'` or `'N'`
- [ ] `TransactionTypes.TypeCode` values match mainframe lookup table exactly
- [ ] `TransactionCategories` composite keys match mainframe `TRANCATG` entries

### Parallel-run comparison

- [ ] Bidirectional conversion test: mainframe → Azure SQL → mainframe produces identical bytes
- [ ] Transaction posting in parallel: daily batch results match between COBOL (CBTRN02C) and Azure Function
- [ ] Reject counts and codes match between mainframe and `DailyRejects` table
- [ ] Category balance updates match between COBOL (CBTRN02C) and .NET `TransactionPostingService`
