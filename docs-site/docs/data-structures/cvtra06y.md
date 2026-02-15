---
id: DS-DTRAN-001
title: "Daily Transaction Record"
copybook_name: "CVTRA06Y.cpy"
domain: "transactions"
used_by_programs: [CBTRN01C, CBTRN02C, CBTRN03C, CORPT00C]
record_length: 350
status: "extracted"
target_schema: "dbo.DailyTransaction"
sidebar_position: 11
---

# DS-DTRAN-001: Daily Transaction Record (CVTRA06Y)

## Overview

The `CVTRA06Y.cpy` copybook defines the **Daily Transaction Record**, which has the same field layout as the Transaction Record (CVTRA05Y) but is stored in a separate VSAM file (`DALYTRAN`) used specifically for daily batch processing. This structure supports daily transaction reports, end-of-day batch reconciliation, and nightly processing runs.

The separation of daily transactions into a dedicated file allows batch programs to process only the current day's transactions without scanning the full historical transaction file, which is a common mainframe performance optimization pattern.

**Source file:** `CVTRA06Y.cpy`
**Record length:** 350 bytes
**Used by:** `CBTRN01C`, `CBTRN02C`, `CBTRN03C`, `CORPT00C`
**VSAM file:** `DALYTRAN`

## Source COBOL

```cobol
01  DALYTRAN-RECORD.
    05  DALYTRAN-ID                             PIC X(16).
    05  DALYTRAN-TYPE-CD                        PIC X(02).
    05  DALYTRAN-CAT-CD                         PIC 9(04).
    05  DALYTRAN-SOURCE                         PIC X(10).
    05  DALYTRAN-DESC                           PIC X(100).
    05  DALYTRAN-AMT                            PIC S9(09)V99.
    05  DALYTRAN-MERCHANT-ID                    PIC 9(09).
    05  DALYTRAN-MERCHANT-NAME                  PIC X(50).
    05  DALYTRAN-MERCHANT-CITY                  PIC X(50).
    05  DALYTRAN-MERCHANT-ZIP                   PIC X(10).
    05  DALYTRAN-CARD-NUM                       PIC X(16).
    05  DALYTRAN-ORIG-TS                        PIC X(26).
    05  DALYTRAN-PROC-TS                        PIC X(26).
    05  FILLER                                  PIC X(20).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `DALYTRAN-ID` | `X(16)` | Alphanumeric | 0 | 16 | Transaction unique identifier |
| 2 | `DALYTRAN-TYPE-CD` | `X(02)` | Alphanumeric | 16 | 2 | Transaction type code |
| 3 | `DALYTRAN-CAT-CD` | `9(04)` | Numeric (display) | 18 | 4 | Transaction category code |
| 4 | `DALYTRAN-SOURCE` | `X(10)` | Alphanumeric | 22 | 10 | Transaction source |
| 5 | `DALYTRAN-DESC` | `X(100)` | Alphanumeric | 32 | 100 | Transaction description |
| 6 | `DALYTRAN-AMT` | `S9(09)V99` | Signed numeric with 2 implied decimals | 132 | 11 | Transaction amount |
| 7 | `DALYTRAN-MERCHANT-ID` | `9(09)` | Numeric (display) | 143 | 9 | Merchant identifier |
| 8 | `DALYTRAN-MERCHANT-NAME` | `X(50)` | Alphanumeric | 152 | 50 | Merchant name |
| 9 | `DALYTRAN-MERCHANT-CITY` | `X(50)` | Alphanumeric | 202 | 50 | Merchant city |
| 10 | `DALYTRAN-MERCHANT-ZIP` | `X(10)` | Alphanumeric | 252 | 10 | Merchant ZIP/postal code |
| 11 | `DALYTRAN-CARD-NUM` | `X(16)` | Alphanumeric | 262 | 16 | Card number (PAN) |
| 12 | `DALYTRAN-ORIG-TS` | `X(26)` | Alphanumeric | 278 | 26 | Origination timestamp |
| 13 | `DALYTRAN-PROC-TS` | `X(26)` | Alphanumeric | 304 | 26 | Processing timestamp |
| 14 | `FILLER` | `X(20)` | Filler | 330 | 20 | Reserved/unused space |

**Total record length:** 350 bytes

## Field Notes

1. **DALYTRAN-ID** -- 16-character unique transaction identifier. Same format as `TRAN-ID` in CVTRA05Y. Records in the daily file correspond to records in the main transaction file. The daily file may be cleared/rebuilt each day.

2. **DALYTRAN-TYPE-CD** -- 2-character transaction type code. Foreign key to Transaction Type (CVTRA03Y).

3. **DALYTRAN-CAT-CD** -- 4-digit numeric category code. Combined with type code, foreign key to Transaction Category (CVTRA04Y).

4. **DALYTRAN-SOURCE** -- 10-character source identifier (POS, ATM, ONLINE, BATCH, etc.). Trim trailing spaces.

5. **DALYTRAN-DESC** -- 100-character transaction description. Trim trailing spaces. May contain Swedish characters.

6. **DALYTRAN-AMT** -- Signed numeric with 2 implied decimal places. Same format and considerations as `TRAN-AMT` in CVTRA05Y. Critical for daily reconciliation totals.

7. **DALYTRAN-MERCHANT-ID** -- 9-digit numeric merchant identifier. Preserve leading zeros.

8. **DALYTRAN-MERCHANT-NAME** -- 50-character merchant name. Trim trailing spaces. May contain Swedish characters.

9. **DALYTRAN-MERCHANT-CITY** -- 50-character merchant city. Trim trailing spaces. May contain Swedish characters.

10. **DALYTRAN-MERCHANT-ZIP** -- 10-character postal code.

11. **DALYTRAN-CARD-NUM** -- 16-character card number (PAN). **PCI-DSS sensitive field.** Same handling requirements as `TRAN-CARD-NUM` in CVTRA05Y. Must be encrypted at rest and masked in display.

12. **DALYTRAN-ORIG-TS** -- 26-character origination timestamp string. Likely ISO 8601 format. Parse to `DATETIME2`.

13. **DALYTRAN-PROC-TS** -- 26-character processing timestamp string. All records in the daily file should have processing timestamps within the same business day.

14. **FILLER** -- 20 bytes reserved. Verify contents before discarding.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration |
|-------|---------------------|
| `DALYTRAN-ID` | Standard EBCDIC-to-UTF-8 conversion. Preserve exact value. |
| `DALYTRAN-TYPE-CD` | Standard EBCDIC-to-UTF-8 conversion. |
| `DALYTRAN-CAT-CD` | Zoned decimal. Convert EBCDIC digits to ASCII. |
| `DALYTRAN-SOURCE` | Standard EBCDIC-to-UTF-8 conversion. Trim trailing spaces. |
| `DALYTRAN-DESC` | EBCDIC-to-UTF-8. Swedish characters possible (CCSID 278). Trim trailing spaces. |
| `DALYTRAN-AMT` | Signed zoned decimal. Sign in last byte zone nibble: `C` = positive, `D` = negative. |
| `DALYTRAN-MERCHANT-ID` | Zoned decimal. Convert to ASCII. Preserve leading zeros. |
| `DALYTRAN-MERCHANT-NAME` | EBCDIC-to-UTF-8. Swedish characters possible. Trim trailing spaces. |
| `DALYTRAN-MERCHANT-CITY` | EBCDIC-to-UTF-8. Swedish characters possible. Trim trailing spaces. |
| `DALYTRAN-MERCHANT-ZIP` | Standard EBCDIC-to-UTF-8 conversion. Trim trailing spaces. |
| `DALYTRAN-CARD-NUM` | Standard EBCDIC-to-UTF-8 conversion. **PCI-DSS: handle in secure memory.** |
| `DALYTRAN-ORIG-TS` | Standard EBCDIC-to-UTF-8. Parse to `DATETIME2` after conversion. |
| `DALYTRAN-PROC-TS` | Standard EBCDIC-to-UTF-8. Parse to `DATETIME2` after conversion. |
| `FILLER` | Ignore. Verify EBCDIC spaces or low-values. |

## Referential Integrity

```
DALYTRAN-RECORD (CVTRA06Y)
  |
  +-- DALYTRAN-ID ----------> CVTRA05Y.TRAN-ID (Transaction Record)
  |                           Records should exist in both daily and main transaction files.
  |
  +-- DALYTRAN-TYPE-CD -----> CVTRA03Y.TRAN-TYPE (Transaction Type)
  |                           FK: dbo.DailyTransaction.TransactionTypeCd -> dbo.TransactionType.TransactionTypeCd
  |
  +-- DALYTRAN-TYPE-CD +
  |   DALYTRAN-CAT-CD ------> CVTRA04Y.TRAN-CAT-KEY (Transaction Category)
  |                           FK: dbo.DailyTransaction.(TransactionTypeCd, TransactionCategoryCd)
  |                               -> dbo.TransactionCategory.(TransactionTypeCd, TransactionCategoryCd)
  |
  +-- DALYTRAN-CARD-NUM ----> CVACT02Y.ACCT-CARD-NUM (Card Record)
                               FK: dbo.DailyTransaction.CardNumber -> dbo.Card.CardNumberEncrypted
```

## Sample Data

| DALYTRAN-ID | DALYTRAN-TYPE-CD | DALYTRAN-CAT-CD | DALYTRAN-SOURCE | DALYTRAN-DESC | DALYTRAN-AMT | DALYTRAN-MERCHANT-NAME | DALYTRAN-CARD-NUM | DALYTRAN-PROC-TS |
|------------|-----------------|----------------|----------------|--------------|-------------|----------------------|------------------|-----------------|
| `0000000000000042` | `SA` | `5001` | `POS` | `Matinkok` | `+000000089.90` | `Coop Forum` | `4000123456789010` | `2026-02-15-06.30.00.000000` |
| `0000000000000043` | `SA` | `5002` | `ONLINE` | `Online order` | `+000000249.00` | `IKEA Online` | `4000123456789010` | `2026-02-15-08.15.22.000000` |
| `0000000000000044` | `CR` | `6001` | `BATCH` | `Return credit` | `-000000049.90` | `H&M` | `4000987654321098` | `2026-02-15-06.30.00.000000` |

> **Note:** All records in a daily file share the same business date (processing date). `DALYTRAN-CARD-NUM` contains real PANs in production -- handle per PCI-DSS.

## Migration Notes

### Target DDL

```sql
CREATE TABLE dbo.DailyTransaction (
    TransactionId         VARCHAR(16)    NOT NULL,
    TransactionTypeCd     CHAR(2)        NOT NULL,
    TransactionCategoryCd INT            NOT NULL,
    TransactionSource     VARCHAR(10)    NULL,
    TransactionDesc       NVARCHAR(100)  NULL,
    TransactionAmount     DECIMAL(11,2)  NOT NULL,
    MerchantId            BIGINT         NULL,
    MerchantName          NVARCHAR(50)   NULL,
    MerchantCity          NVARCHAR(50)   NULL,
    MerchantZip           VARCHAR(10)    NULL,
    CardNumber            VARBINARY(256) NOT NULL,  -- Encrypted PAN (PCI-DSS)
    OriginationTimestamp  DATETIME2(6)   NOT NULL,
    ProcessingTimestamp   DATETIME2(6)   NOT NULL,

    CONSTRAINT PK_DailyTransaction
        PRIMARY KEY (TransactionId),

    CONSTRAINT FK_DailyTrans_TransType
        FOREIGN KEY (TransactionTypeCd)
        REFERENCES dbo.TransactionType (TransactionTypeCd),

    CONSTRAINT FK_DailyTrans_TransCategory
        FOREIGN KEY (TransactionTypeCd, TransactionCategoryCd)
        REFERENCES dbo.TransactionCategory (TransactionTypeCd, TransactionCategoryCd),

    CONSTRAINT FK_DailyTrans_Card
        FOREIGN KEY (CardNumber)
        REFERENCES dbo.Card (CardNumberEncrypted)
);

-- Index for daily batch processing queries
CREATE INDEX IX_DailyTransaction_ProcTimestamp
    ON dbo.DailyTransaction (ProcessingTimestamp);

-- Index for card-based daily lookups
CREATE INDEX IX_DailyTransaction_CardNumber
    ON dbo.DailyTransaction (CardNumber);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `DALYTRAN-ID` | `PIC X(16)` | `VARCHAR(16)` | `string` | Primary key |
| `DALYTRAN-TYPE-CD` | `PIC X(02)` | `CHAR(2)` | `string` | FK to TransactionType |
| `DALYTRAN-CAT-CD` | `PIC 9(04)` | `INT` | `int` | FK (with type) to TransactionCategory |
| `DALYTRAN-SOURCE` | `PIC X(10)` | `VARCHAR(10)` | `string` | Trim trailing spaces |
| `DALYTRAN-DESC` | `PIC X(100)` | `NVARCHAR(100)` | `string` | NVARCHAR for Swedish characters |
| `DALYTRAN-AMT` | `PIC S9(09)V99` | `DECIMAL(11,2)` | `decimal` | Signed, 2 implied decimals |
| `DALYTRAN-MERCHANT-ID` | `PIC 9(09)` | `BIGINT` | `long` | 9-digit numeric |
| `DALYTRAN-MERCHANT-NAME` | `PIC X(50)` | `NVARCHAR(50)` | `string` | Swedish characters |
| `DALYTRAN-MERCHANT-CITY` | `PIC X(50)` | `NVARCHAR(50)` | `string` | Swedish characters |
| `DALYTRAN-MERCHANT-ZIP` | `PIC X(10)` | `VARCHAR(10)` | `string` | Postal codes |
| `DALYTRAN-CARD-NUM` | `PIC X(16)` | `VARBINARY(256)` | `byte[]` | Encrypted PAN (PCI-DSS) |
| `DALYTRAN-ORIG-TS` | `PIC X(26)` | `DATETIME2(6)` | `DateTime` | Parse from string |
| `DALYTRAN-PROC-TS` | `PIC X(26)` | `DATETIME2(6)` | `DateTime` | Parse from string |

### Migration Architecture Note

In the target .NET/Azure architecture, the separation of daily transactions into a distinct file may not be necessary. Consider:

1. **Option A: Maintain separate table** -- Keep `dbo.DailyTransaction` as a staging table that is populated daily and cleared after batch processing. This mirrors the mainframe pattern and simplifies parallel-run comparison.

2. **Option B: Use partitioned view/index** -- Store all transactions in `dbo.Transaction` with a date-based partition. Daily batch processing queries filter by `ProcessingTimestamp`. This eliminates data duplication.

**Recommendation for parallel-run period:** Use Option A to maintain 1:1 correspondence with the mainframe. After parallel-run validation is complete, migrate to Option B.

### Post-Migration Validation

```sql
-- Row count comparison (must match mainframe daily file count)
SELECT COUNT(*) AS RowCount FROM dbo.DailyTransaction;

-- Daily total amount (reconcile with mainframe daily batch total)
SELECT SUM(TransactionAmount) AS DailyTotal FROM dbo.DailyTransaction;

-- Verify all daily records exist in main transaction table
SELECT d.TransactionId FROM dbo.DailyTransaction d
LEFT JOIN dbo.[Transaction] t ON d.TransactionId = t.TransactionId
WHERE t.TransactionId IS NULL;

-- Processing date range (should be single business day)
SELECT MIN(ProcessingTimestamp) AS EarliestProc,
       MAX(ProcessingTimestamp) AS LatestProc
FROM dbo.DailyTransaction;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS** | Requirement 3: Protect stored cardholder data. Same requirements as CVTRA05Y. | `DALYTRAN-CARD-NUM` must be encrypted (AES-256 / Always Encrypted). Masked in display. Access logged. Applies even though daily file is temporary -- PAN at rest must always be encrypted. |
| **PCI-DSS** | Requirement 10: Track access to cardholder data. | Audit logging for all access to daily transaction card numbers. |
| **GDPR** | Personal data processing. Daily batch processing constitutes data processing under GDPR. | Processing activities must be documented in the GDPR processing register. Data minimisation -- only fields needed for daily processing should be included. |
| **FSA (FFFS 2014:5)** | Nightly batch processing SLA. Daily reconciliation is a regulatory control. | Daily batch must complete by 06:00 SLA. Reconciliation between daily and main transaction files is a key internal control. Failures must trigger alerts. |
| **AML/KYC** | Nightly AML screening uses daily transaction data. | Daily transaction file feeds into AML screening batch. Must be available before AML screening job starts. Processing order: daily transaction load -> AML screening -> reporting. |
| **DORA** | ICT operational resilience. Daily batch processing is a critical function. | Include daily batch in business continuity and disaster recovery plans. Monitor batch completion times. Implement automatic retry and alerting for failures. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
