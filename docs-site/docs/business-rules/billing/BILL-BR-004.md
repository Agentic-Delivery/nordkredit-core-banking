---
id: "BILL-BR-004"
title: "Transaction record layout for statement reporting uses card-first key structure"
domain: "billing"
cobol_source: "COSTM01.CPY:1-38"
requirement_id: "BILL-BR-004"
regulations:
  - "PSD2 Art. 57 — Itemized Transaction Information"
  - "FFFS 2014:5 Ch. 4 §5 — Statement Content Requirements"
  - "GDPR Art. 5(1)(e) — Storage Limitation"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# BILL-BR-004: Transaction record layout for statement reporting uses card-first key structure

## Summary

The COSTM01 copybook defines the transaction record layout used by the statement generation process. The key difference from the standard transaction record (CVTRA05Y) is that the composite key is ordered as card number + transaction ID (rather than transaction ID first). This layout is used after the CREASTMT JCL SORT step rearranges transaction records for efficient per-card statement processing.

## Business Logic

### Data Structure

```
TRNX-RECORD (total: 350 bytes)
├── TRNX-KEY (32 bytes)
│   ├── TRNX-CARD-NUM       PIC X(16)    Card number (primary sort key)
│   └── TRNX-ID             PIC X(16)    Transaction ID (secondary sort key)
├── TRNX-REST (318 bytes)
│   ├── TRNX-TYPE-CD        PIC X(02)    Transaction type code
│   ├── TRNX-CAT-CD         PIC 9(04)    Transaction category code
│   ├── TRNX-SOURCE         PIC X(10)    Transaction source
│   ├── TRNX-DESC           PIC X(100)   Transaction description
│   ├── TRNX-AMT            PIC S9(09)V99  Transaction amount (signed, 2 dec)
│   ├── TRNX-MERCHANT-ID    PIC 9(09)    Merchant identifier
│   ├── TRNX-MERCHANT-NAME  PIC X(50)    Merchant name
│   ├── TRNX-MERCHANT-CITY  PIC X(50)    Merchant city
│   ├── TRNX-MERCHANT-ZIP   PIC X(10)    Merchant ZIP code
│   ├── TRNX-ORIG-TS        PIC X(26)    Original timestamp
│   ├── TRNX-PROC-TS        PIC X(26)    Processing timestamp
│   └── FILLER              PIC X(20)    Reserved/unused
```

### Key Differences from Standard Transaction Layout

| Aspect | Standard (CVTRA05Y) | Statement (COSTM01) |
|--------|--------------------|--------------------|
| Primary key | Transaction ID (16 bytes) | Card Number (16 bytes) |
| Secondary key | Card Number (after ID) | Transaction ID (after card) |
| Sort order | By transaction ID | By card number, then transaction ID |
| Usage | Online CICS transaction processing | Batch statement generation |

### Field Details

| Field | Type | Size | Description | Constraints |
|-------|------|------|-------------|-------------|
| TRNX-CARD-NUM | Alphanumeric | 16 | Card number | Primary key component |
| TRNX-ID | Alphanumeric | 16 | Transaction ID | Secondary key component |
| TRNX-TYPE-CD | Alphanumeric | 2 | Type code: '01' = system/interest, '02' = payment | Known values: '01', '02' |
| TRNX-CAT-CD | Numeric | 4 | Category code | Known values: 2 (payment), 5 (interest) |
| TRNX-SOURCE | Alphanumeric | 10 | Origin: 'POS TERM', 'System' | Free text |
| TRNX-DESC | Alphanumeric | 100 | Description text | Free text |
| TRNX-AMT | Signed decimal | 11 (9+2) | Amount with 2 decimal places | Can be negative (credits) |
| TRNX-MERCHANT-ID | Numeric | 9 | Merchant ID | 999999999 = system payment |
| TRNX-MERCHANT-NAME | Alphanumeric | 50 | Merchant name | 'BILL PAYMENT' for system |
| TRNX-MERCHANT-CITY | Alphanumeric | 50 | Merchant city | 'N/A' for system |
| TRNX-MERCHANT-ZIP | Alphanumeric | 10 | Merchant ZIP | 'N/A' for system |
| TRNX-ORIG-TS | Alphanumeric | 26 | Original timestamp (DB2 format) | YYYY-MM-DD-HH.MM.SS.NNNNNN |
| TRNX-PROC-TS | Alphanumeric | 26 | Processing timestamp (DB2 format) | YYYY-MM-DD-HH.MM.SS.NNNNNN |
| FILLER | Alphanumeric | 20 | Reserved | Unused |

### Transaction Type Codes (from COBIL00C and CBACT04C)

| Type Code | Category Code | Source | Description |
|-----------|--------------|--------|-------------|
| '01' | 05 | System | Interest charges (from CBACT04C) |
| '02' | 0002 | POS TERM | Bill payment - online (from COBIL00C) |

## Source COBOL Reference

**Copybook:** `COSTM01.CPY`
**Lines:** 1-38

```cobol
      ******************************************************************
      * CardDemo - Transaction altered Layout for use in reporting
      ******************************************************************
       01  TRNX-RECORD.
           05  TRNX-KEY.
               10  TRNX-CARD-NUM                       PIC X(16).
               10  TRNX-ID                             PIC X(16).
           05  TRNX-REST.
               10  TRNX-TYPE-CD                        PIC X(02).
               10  TRNX-CAT-CD                         PIC 9(04).
               10  TRNX-SOURCE                         PIC X(10).
               10  TRNX-DESC                           PIC X(100).
               10  TRNX-AMT                            PIC S9(09)V99.
               10  TRNX-MERCHANT-ID                    PIC 9(09).
               10  TRNX-MERCHANT-NAME                  PIC X(50).
               10  TRNX-MERCHANT-CITY                  PIC X(50).
               10  TRNX-MERCHANT-ZIP                   PIC X(10).
               10  TRNX-ORIG-TS                        PIC X(26).
               10  TRNX-PROC-TS                        PIC X(26).
               10  FILLER                              PIC X(20).
```

**JCL SORT reference (CREASTMT.JCL lines 44-55):**

```jcl
//STEP010  EXEC PGM=SORT
//SORTIN   DD  DISP=SHR,DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS
//SYSIN    DD *
  SORT FIELDS=(263,16,CH,A,1,16,CH,A)
  OUTREC FIELDS=(1:263,16,17:1,262,279:279,50)
```

The SORT rearranges the standard transaction record:
- Bytes 263-278 (card number) move to position 1-16
- Bytes 1-262 (tran ID + data) move to position 17-278
- Bytes 279-328 (remaining) move to position 279-328

This places card number first in the record, matching the COSTM01 layout.

## Acceptance Criteria

### Scenario 1: Transaction record correctly maps all fields

```gherkin
GIVEN a transaction record written by COBIL00C (bill payment):
  | Original Position | Field | Value |
  | 1-16 | TRAN-ID | 0000000000000043 |
  | 263-278 | TRAN-CARD-NUM | 1234567890123456 |
  | 17-18 | TRAN-TYPE-CD | 02 |
  | 19-22 | TRAN-CAT-CD | 0002 |
  | 23-32 | TRAN-SOURCE | POS TERM |
  | 33-132 | TRAN-DESC | BILL PAYMENT - ONLINE |
  | 133-143 | TRAN-AMT | 00001500.00 |
WHEN the SORT step rearranges the record into COSTM01 layout
THEN the resulting record has:
  | Position | Field | Value |
  | 1-16 | TRNX-CARD-NUM | 1234567890123456 |
  | 17-32 | TRNX-ID | 0000000000000043 |
  | 33-34 | TRNX-TYPE-CD | 02 |
  | 35-38 | TRNX-CAT-CD | 0002 |
  | 39-48 | TRNX-SOURCE | POS TERM |
  | 49-148 | TRNX-DESC | BILL PAYMENT - ONLINE |
  | 149-159 | TRNX-AMT | +00001500.00 |
```

### Scenario 2: Signed amount encoding preserved through SORT

```gherkin
GIVEN a transaction with a negative amount (credit/refund) of -250.75
WHEN the SORT rearranges the record
THEN the TRNX-AMT field preserves the sign: S9(09)V99 = -000000250.75
  AND the sign is stored in the COBOL standard zoned decimal format
```

### Scenario 3: FILLER field preserved

```gherkin
GIVEN a 350-byte transaction record
WHEN mapped to the COSTM01 layout
THEN bytes 331-350 map to the 20-byte FILLER field
  AND these bytes are preserved but not used in statement generation
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 57(1) | Statements must include reference, amount, charges, date, and description for each transaction | The record layout includes transaction ID (reference), amount, description, timestamps, and merchant details. |
| FFFS 2014:5 | Ch. 4 §5 | Statement content must enable customer to identify transactions | Merchant name, city, and description fields provide identifying information for each transaction. |
| GDPR | Art. 5(1)(e) | Data stored only as long as necessary | The FILLER field (20 bytes) contains no PII. The migration should not persist unused fields. |

## Edge Cases

1. **EBCDIC signed decimal**: `TRNX-AMT` with `PIC S9(09)V99` stores the sign in the rightmost byte's zone nibble (EBCDIC convention). The SORT preserves the raw bytes. The migration must handle EBCDIC-to-Unicode conversion for this field, converting the sign representation properly.

2. **Timestamp format**: `TRNX-ORIG-TS` and `TRNX-PROC-TS` use DB2 timestamp format: `YYYY-MM-DD-HH.MM.SS.NNNNNN` (26 characters). The migration should convert to ISO 8601 format (`YYYY-MM-DDTHH:MM:SS.ffffff`).

3. **FILLER bytes**: The 20-byte FILLER at the end of the record is carried through the SORT but never used. The migration should not include this field in the target data model.

4. **Record size**: The record is fixed at 350 bytes (matching the VSAM KSDS definition: `RECORDSIZE(350 350)`). The migration to Azure SQL uses variable-length columns, so the fixed-size constraint no longer applies.

## Domain Expert Notes

- **Pending validation**: This rule documents a data structure, not a business rule per se, but the layout is critical for understanding how statement generation works and how the migration must handle the SORT-based record rearrangement.
- In the target system, the SORT step and record rearrangement would be replaced by a SQL query with `ORDER BY card_number, transaction_id`, eliminating the need for a separate intermediate file.
- The separation of the reporting layout (COSTM01) from the transaction processing layout (CVTRA05Y) is a common mainframe pattern. The migration should use a single transaction table with appropriate indexes.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
