---
id: "trn-br-007"
title: "Transaction record data structures"
domain: "transactions"
cobol_source: "CVTRA05Y.cpy:1-21,CVTRA06Y.cpy:1-22"
requirement_id: "TRN-BR-007"
regulations:
  - "PSD2 Art. 64"
  - "GDPR Art. 5"
  - "FSA FFFS 2014:5"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-007: Transaction record data structures

## Summary

The transaction processing domain uses two primary record structures: the posted transaction record (TRAN-RECORD, defined in CVTRA05Y.cpy) and the daily transaction input record (DALYTRAN-RECORD, defined in CVTRA06Y.cpy). Both structures are 350 bytes and share identical field layouts, representing the transaction lifecycle from daily input through to posted storage. These copybooks are referenced by all six transaction processing programs (COTRN00C, COTRN01C, COTRN02C, CBTRN01C, CBTRN02C, CBTRN03C).

## Field Specifications

### TRAN-RECORD (CVTRA05Y.cpy) — Posted Transaction Record

| Field | COBOL Name | PIC | Offset | Length | Description |
|---|---|---|---|---|---|
| Transaction ID | TRAN-ID | X(16) | 1 | 16 | Unique sequential identifier |
| Type Code | TRAN-TYPE-CD | X(02) | 17 | 2 | Transaction type classification |
| Category Code | TRAN-CAT-CD | 9(04) | 19 | 4 | Transaction category classification |
| Source | TRAN-SOURCE | X(10) | 23 | 10 | Transaction origination source |
| Description | TRAN-DESC | X(100) | 33 | 100 | Free-text transaction description |
| Amount | TRAN-AMT | S9(09)V99 | 133 | 11 | Signed amount with 2 decimal places |
| Merchant ID | TRAN-MERCHANT-ID | 9(09) | 144 | 9 | Merchant identifier |
| Merchant Name | TRAN-MERCHANT-NAME | X(50) | 153 | 50 | Merchant business name |
| Merchant City | TRAN-MERCHANT-CITY | X(50) | 203 | 50 | Merchant city |
| Merchant ZIP | TRAN-MERCHANT-ZIP | X(10) | 253 | 10 | Merchant postal code |
| Card Number | TRAN-CARD-NUM | X(16) | 263 | 16 | Associated credit card number |
| Origination TS | TRAN-ORIG-TS | X(26) | 279 | 26 | When transaction was originated |
| Processing TS | TRAN-PROC-TS | X(26) | 305 | 26 | When transaction was processed/posted |
| Filler | FILLER | X(20) | 331 | 20 | Reserved/unused |
| **Total** | | | | **350** | |

### DALYTRAN-RECORD (CVTRA06Y.cpy) — Daily Transaction Input Record

| Field | COBOL Name | PIC | Offset | Length | Description |
|---|---|---|---|---|---|
| Transaction ID | DALYTRAN-ID | X(16) | 1 | 16 | Transaction identifier from source |
| Type Code | DALYTRAN-TYPE-CD | X(02) | 17 | 2 | Transaction type classification |
| Category Code | DALYTRAN-CAT-CD | 9(04) | 19 | 4 | Transaction category classification |
| Source | DALYTRAN-SOURCE | X(10) | 23 | 10 | Transaction origination source |
| Description | DALYTRAN-DESC | X(100) | 33 | 100 | Free-text transaction description |
| Amount | DALYTRAN-AMT | S9(09)V99 | 133 | 11 | Signed amount with 2 decimal places |
| Merchant ID | DALYTRAN-MERCHANT-ID | 9(09) | 144 | 9 | Merchant identifier |
| Merchant Name | DALYTRAN-MERCHANT-NAME | X(50) | 153 | 50 | Merchant business name |
| Merchant City | DALYTRAN-MERCHANT-CITY | X(50) | 203 | 50 | Merchant city |
| Merchant ZIP | DALYTRAN-MERCHANT-ZIP | X(10) | 253 | 10 | Merchant postal code |
| Card Number | DALYTRAN-CARD-NUM | X(16) | 263 | 16 | Associated credit card number |
| Origination TS | DALYTRAN-ORIG-TS | X(26) | 279 | 26 | When transaction was originated |
| Processing TS | DALYTRAN-PROC-TS | X(26) | 305 | 26 | When transaction was processed |
| Filler | FILLER | X(20) | 331 | 20 | Reserved/unused |
| **Total** | | | | **350** | |

### Supporting Data Structures

#### TRAN-CAT-BAL-RECORD (CVTRA01Y.cpy) — Transaction Category Balance

| Field | COBOL Name | PIC | Length | Description |
|---|---|---|---|---|
| Account ID | TRANCAT-ACCT-ID | 9(11) | 11 | Account identifier |
| Type Code | TRANCAT-TYPE-CD | X(02) | 2 | Transaction type code |
| Category Code | TRANCAT-CD | 9(04) | 4 | Transaction category code |
| Balance | TRAN-CAT-BAL | S9(09)V99 | 11 | Running balance for this category |
| Filler | FILLER | X(22) | 22 | Reserved |
| **Total** | | | **50** | |

#### DIS-GROUP-RECORD (CVTRA02Y.cpy) — Disclosure Group

| Field | COBOL Name | PIC | Length | Description |
|---|---|---|---|---|
| Account Group ID | DIS-ACCT-GROUP-ID | X(10) | 10 | Disclosure group identifier |
| Transaction Type | DIS-TRAN-TYPE-CD | X(02) | 2 | Transaction type code |
| Category Code | DIS-TRAN-CAT-CD | 9(04) | 4 | Transaction category code |
| Interest Rate | DIS-INT-RATE | S9(04)V99 | 6 | Interest rate for this group |
| Filler | FILLER | X(28) | 28 | Reserved |
| **Total** | | | **50** | |

#### TRAN-TYPE-RECORD (CVTRA03Y.cpy) — Transaction Type

| Field | COBOL Name | PIC | Length | Description |
|---|---|---|---|---|
| Type Code | TRAN-TYPE | X(02) | 2 | Transaction type code |
| Description | TRAN-TYPE-DESC | X(50) | 50 | Type description |
| Filler | FILLER | X(08) | 8 | Reserved |
| **Total** | | | **60** | |

#### TRAN-CAT-RECORD (CVTRA04Y.cpy) — Transaction Category Type

| Field | COBOL Name | PIC | Length | Description |
|---|---|---|---|---|
| Type Code | TRAN-TYPE-CD | X(02) | 2 | Transaction type code |
| Category Code | TRAN-CAT-CD | 9(04) | 4 | Category code |
| Description | TRAN-CAT-TYPE-DESC | X(50) | 50 | Category type description |
| Filler | FILLER | X(04) | 4 | Reserved |
| **Total** | | | **60** | |

#### REPORT structures (CVTRA07Y.cpy) — Report Layout

| Structure | Purpose | Width |
|---|---|---|
| REPORT-NAME-HEADER | Report title with date range | 115 chars |
| TRANSACTION-DETAIL-REPORT | Detail line with all fields | 133 chars |
| TRANSACTION-HEADER-1 | Column labels | 114 chars |
| TRANSACTION-HEADER-2 | Separator line (all dashes) | 133 chars |
| REPORT-PAGE-TOTALS | Page total line | 111 chars |
| REPORT-ACCOUNT-TOTALS | Account total line | 111 chars |
| REPORT-GRAND-TOTALS | Grand total line | 111 chars |

## VSAM File Organization

| File | Key Field | Key Length | Record Length | Access Method | Purpose |
|---|---|---|---|---|---|
| TRANSACT | TRAN-ID | 16 | 350 | KSDS (key sequential) | Posted transactions |
| DALYTRAN | N/A | N/A | 350 | Sequential (ESDS) | Daily transaction input |
| TCATBALF | ACCT-ID+TYPE-CD+CAT-CD | 17 | 50 | KSDS (key sequential) | Category balances |
| TRANTYPE | TRAN-TYPE | 2 | 60 | KSDS (key sequential) | Type descriptions |
| TRANCATG | TYPE-CD+CAT-CD | 6 | 60 | KSDS (key sequential) | Category descriptions |
| DALYREJS | N/A | N/A | 430 | Sequential | Rejected transactions |
| TRANREPT | N/A | N/A | 133 | Sequential | Report output |

## Usage in Programs

- `COTRN00C.cbl` (line 78): `COPY CVTRA05Y.` — Transaction list display
- `COTRN01C.cbl` (line 69): `COPY CVTRA05Y.` — Transaction detail view
- `COTRN02C.cbl` (line 88): `COPY CVTRA05Y.` — Transaction add
- `CBTRN01C.cbl` (lines 99, 124): `COPY CVTRA06Y.` and `COPY CVTRA05Y.` — Batch ingestion
- `CBTRN02C.cbl` (lines 102, 107, 126): `COPY CVTRA06Y.`, `COPY CVTRA05Y.`, `COPY CVTRA01Y.` — Batch posting
- `CBTRN03C.cbl` (lines 93, 103, 108, 113): `COPY CVTRA05Y.`, `CVTRA03Y.`, `CVTRA04Y.`, `CVTRA07Y.` — Report generation

## Source COBOL Reference

**Copybook:** `CVTRA05Y.cpy`
**Lines:** 1-21

```cobol
      *    Data-structure for TRANsaction record (RECLN = 350)
       01  TRAN-RECORD.
           05  TRAN-ID                                 PIC X(16).
           05  TRAN-TYPE-CD                            PIC X(02).
           05  TRAN-CAT-CD                             PIC 9(04).
           05  TRAN-SOURCE                             PIC X(10).
           05  TRAN-DESC                               PIC X(100).
           05  TRAN-AMT                                PIC S9(09)V99.
           05  TRAN-MERCHANT-ID                        PIC 9(09).
           05  TRAN-MERCHANT-NAME                      PIC X(50).
           05  TRAN-MERCHANT-CITY                      PIC X(50).
           05  TRAN-MERCHANT-ZIP                       PIC X(10).
           05  TRAN-CARD-NUM                           PIC X(16).
           05  TRAN-ORIG-TS                            PIC X(26).
           05  TRAN-PROC-TS                            PIC X(26).
           05  FILLER                                  PIC X(20).
```

## Acceptance Criteria

### Scenario 1: Transaction record field mapping

GIVEN the TRAN-RECORD copybook (CVTRA05Y.cpy)
WHEN a transaction is stored in the TRANSACT file
THEN it occupies exactly 350 bytes
  AND each field is at the documented offset and length

### Scenario 2: Daily transaction record compatibility

GIVEN the DALYTRAN-RECORD copybook (CVTRA06Y.cpy)
  AND the TRAN-RECORD copybook (CVTRA05Y.cpy)
WHEN a daily transaction is mapped to a posted transaction
THEN all fields align at the same offsets and lengths
  AND a direct MOVE from DALYTRAN fields to TRAN fields preserves data

### Scenario 3: Amount field precision

GIVEN TRAN-AMT is defined as S9(09)V99
WHEN a transaction amount is stored
THEN it supports values from -999999999.99 to +999999999.99
  AND the implied decimal point provides exactly 2 decimal places
  AND no rounding occurs during storage

### Scenario 4: Category balance key structure

GIVEN the TRAN-CAT-BAL-RECORD (CVTRA01Y.cpy)
WHEN a category balance is looked up
THEN the composite key is ACCT-ID(11) + TYPE-CD(2) + CAT-CD(4) = 17 bytes
  AND each unique combination of account, type, and category has one balance record

### Scenario 5: Timestamp format

GIVEN TRAN-ORIG-TS and TRAN-PROC-TS are PIC X(26)
WHEN timestamps are stored
THEN they use DB2 format: YYYY-MM-DD-HH.MM.SS.HH0000
  AND date comparisons using the first 10 characters (YYYY-MM-DD) preserve chronological order

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 64 | Transaction data completeness | Record structure captures all required transaction attributes including merchant data and timestamps |
| GDPR | Art. 5 | Data minimization | Record structure includes only operationally necessary fields; 20-byte filler suggests planned fields were not implemented |
| FSA FFFS 2014:5 | Ch. 7 | Financial record structure | Fixed-precision numeric fields ensure consistent storage of financial amounts |

## Edge Cases

1. **EBCDIC encoding**: All PIC X fields are stored in EBCDIC on the mainframe. The migration must convert to Unicode (UTF-8) with proper handling of Swedish characters (å, ä, ö) in merchant names and descriptions.

2. **Implied decimal point**: TRAN-AMT uses `S9(09)V99` with an implied (not stored) decimal point. The 11-byte storage holds a signed numeric without an explicit decimal character. The migrated system must use `decimal(11,2)` to match.

3. **Timestamp as string**: Both timestamps are stored as PIC X(26) strings, not native date types. The migrated system should use native `datetime2` (SQL Server) or `timestamptz` (PostgreSQL) types with conversion during data migration.

4. **Filler bytes**: The 20-byte FILLER at the end of both records suggests reserved space for future fields. The migrated system does not need to preserve this padding.

5. **Composite keys**: The TCATBAL key is a concatenation of three fields (ACCT-ID + TYPE-CD + CAT-CD). In the migrated database, this should be modeled as a composite primary key or a surrogate key with a unique constraint on the three columns.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Questions: (1) What are the valid transaction type codes and their meanings? (2) What are the valid category codes? (3) Is the DIS-GROUP-RECORD (disclosure/interest rate) actively used in current processing? (4) Should the 20-byte filler be reserved for any planned extensions?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
