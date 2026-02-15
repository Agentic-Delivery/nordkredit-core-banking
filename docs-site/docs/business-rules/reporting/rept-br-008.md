---
id: "rept-br-008"
title: "Customer data export for branch migration"
domain: "reporting"
cobol_source: "CBEXPORT.cbl:1-583"
requirement_id: "REPT-BR-008"
regulations:
  - "GDPR Art. 5 (data minimization in exports)"
  - "GDPR Art. 44-49 (data transfer)"
  - "FFFS 2014:5 (operational continuity)"
  - "DORA Art. 11 (ICT change management)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# REPT-BR-008: Customer data export for branch migration

## Summary

The CBEXPORT batch program reads five normalized CardDemo indexed VSAM files (customers, accounts, cross-references, transactions, and cards) and produces a single multi-record export file with a unified 500-byte fixed-length record layout. The program processes each record type sequentially in a fixed order: customers first, then accounts, cross-references, transactions, and finally cards. For each source record, the program creates an export record containing a record type indicator ('C' for customer, 'A' for account, 'X' for cross-reference, 'T' for transaction, 'D' for card), a 26-character formatted timestamp, an incrementing sequence number that serves as the export file key, a branch ID ('0001'), and a region code ('NORTH'), followed by the mapped source fields in a type-specific payload area. The program tracks import statistics per record type and performs file status checks on every I/O operation, invoking CEE3ABD for abnormal termination on any file error. This export mechanism supports branch migration scenarios and must comply with GDPR data transfer requirements given the presence of PII fields such as SSN, government ID, and date of birth.

## Business Logic

### Pseudocode

```
FUNCTION ExportCustomerData():
    // Main processing flow (0000-MAIN-PROCESSING)
    CALL Initialize()
    CALL ExportCustomers()
    CALL ExportAccounts()
    CALL ExportXRefs()
    CALL ExportTransactions()
    CALL ExportCards()
    CALL Finalize()

FUNCTION Initialize():
    sequenceNumber = 0
    customerCount = 0
    accountCount = 0
    xrefCount = 0
    transactionCount = 0
    cardCount = 0
    totalCount = 0
    timestamp = FORMAT_CURRENT_DATETIME("YYYY-MM-DD-HH.MM.SS.NNNNNN")
    OPEN OUTPUT exportFile

FUNCTION ExportCustomers():
    OPEN INPUT customerFile (indexed by CUST-ID)
    READ FIRST record from customerFile
    WHILE NOT end-of-file:
        INITIALIZE exportRecord
        exportRecord.RecordType = 'C'
        exportRecord.Timestamp = timestamp
        sequenceNumber = sequenceNumber + 1
        exportRecord.SequenceNum = sequenceNumber
        exportRecord.BranchId = '0001'
        exportRecord.RegionCode = 'NORTH'
        // Map customer fields:
        exportRecord.Customer.ID = sourceRecord.CUST-ID
        exportRecord.Customer.FirstName = sourceRecord.CUST-FIRST-NAME
        exportRecord.Customer.MiddleName = sourceRecord.CUST-MIDDLE-NAME
        exportRecord.Customer.LastName = sourceRecord.CUST-LAST-NAME
        exportRecord.Customer.Address1 = sourceRecord.CUST-ADDR-LINE-1
        exportRecord.Customer.Address2 = sourceRecord.CUST-ADDR-LINE-2
        exportRecord.Customer.Address3 = sourceRecord.CUST-ADDR-LINE-3
        exportRecord.Customer.State = sourceRecord.CUST-ADDR-STATE-CD
        exportRecord.Customer.Country = sourceRecord.CUST-ADDR-COUNTRY-CD
        exportRecord.Customer.ZipCode = sourceRecord.CUST-ADDR-ZIP
        exportRecord.Customer.Phone1 = sourceRecord.CUST-PHONE-NUM-1
        exportRecord.Customer.Phone2 = sourceRecord.CUST-PHONE-NUM-2
        exportRecord.Customer.SSN = sourceRecord.CUST-SSN
        exportRecord.Customer.GovtID = sourceRecord.CUST-GOVT-ISSUED-ID
        exportRecord.Customer.DOB = sourceRecord.CUST-DOB
        exportRecord.Customer.EFTAccount = sourceRecord.CUST-EFT-ACCOUNT-ID
        exportRecord.Customer.PrimaryCardholder = sourceRecord.CUST-PRI-CARD-HOLDER-IND
        exportRecord.Customer.FICOScore = sourceRecord.CUST-FICO-CREDIT-SCORE
        WRITE exportRecord TO exportFile
        CHECK file status; IF error THEN ABEND via CEE3ABD
        customerCount = customerCount + 1
        totalCount = totalCount + 1
        READ NEXT record from customerFile
    END WHILE
    CLOSE customerFile

    // ExportAccounts, ExportXRefs, ExportTransactions, ExportCards
    // follow the same pattern with type-specific field mappings

FUNCTION Finalize():
    CLOSE exportFile
    DISPLAY "Export Statistics:"
    DISPLAY "  Customers:    " + customerCount
    DISPLAY "  Accounts:     " + accountCount
    DISPLAY "  XRefs:        " + xrefCount
    DISPLAY "  Transactions: " + transactionCount
    DISPLAY "  Cards:        " + cardCount
    DISPLAY "  Total:        " + totalCount
    STOP RUN
END FUNCTION
```

### Decision Table

| Record Type Indicator | Source File | Source Key | Field Mapping Lines | PII Fields Present |
|-----------------------|-------------|-----------|--------------------|--------------------|
| 'C' (Customer) | CUSTFILE | CUST-ID | 271-310 | SSN, Govt ID, DOB, Phone, Address |
| 'A' (Account) | ACCTFILE | ACCT-ID | 338-373 | No |
| 'X' (Cross-Reference) | XREFFILE | XREF-CARD-NUM | 394-420 | Card number |
| 'T' (Transaction) | TRANSACT | TRAN-ID | 457-493 | Card number, merchant info |
| 'D' (Card) | CARDFILE | CARD-NUM | 522-551 | Card number, CVV, expiry |

## Source COBOL Reference

**Program:** `CBEXPORT.cbl`
**Lines:** 1-583 (Complete batch export program)

Main processing control flow (lines 149-158):
```cobol
000149     PERFORM 1000-INITIALIZE
000150     PERFORM 2000-EXPORT-CUSTOMERS
000151     PERFORM 3000-EXPORT-ACCOUNTS
000152     PERFORM 4000-EXPORT-XREFS
000153     PERFORM 5000-EXPORT-TRANSACTIONS
000154     PERFORM 6000-EXPORT-CARDS
000155     PERFORM 9000-FINALIZE
000156     STOP RUN
000157     .
000158
```

Input file definitions (lines 34-69):
```cobol
000034     SELECT CUST-FILE
000035         ASSIGN TO CUSTFILE
000036         ORGANIZATION IS INDEXED
000037         ACCESS MODE IS SEQUENTIAL
000038         RECORD KEY IS CUST-ID
000039         FILE STATUS IS WS-CUST-FILE-STATUS.
000040
000041     SELECT ACCT-FILE
000042         ASSIGN TO ACCTFILE
000043         ORGANIZATION IS INDEXED
000044         ACCESS MODE IS SEQUENTIAL
000045         RECORD KEY IS ACCT-ID
000046         FILE STATUS IS WS-ACCT-FILE-STATUS.
000047
000048     SELECT XREF-FILE
000049         ASSIGN TO XREFFILE
000050         ORGANIZATION IS INDEXED
000051         ACCESS MODE IS SEQUENTIAL
000052         RECORD KEY IS XREF-CARD-NUM
000053         FILE STATUS IS WS-XREF-FILE-STATUS.
000054
000055     SELECT TRAN-FILE
000056         ASSIGN TO TRANSACT
000057         ORGANIZATION IS INDEXED
000058         ACCESS MODE IS SEQUENTIAL
000059         RECORD KEY IS TRAN-ID
000060         FILE STATUS IS WS-TRAN-FILE-STATUS.
000061
000062     SELECT CARD-FILE
000063         ASSIGN TO CARDFILE
000064         ORGANIZATION IS INDEXED
000065         ACCESS MODE IS SEQUENTIAL
000066         RECORD KEY IS CARD-NUM
000067         FILE STATUS IS WS-CARD-FILE-STATUS.
```

Statistics tracking fields (lines 138-144):
```cobol
000138 01  WS-EXPORT-STATS.
000139     05 WS-CUST-COUNT         PIC 9(7) VALUE ZERO.
000140     05 WS-ACCT-COUNT         PIC 9(7) VALUE ZERO.
000141     05 WS-XREF-COUNT         PIC 9(7) VALUE ZERO.
000142     05 WS-TRAN-COUNT         PIC 9(7) VALUE ZERO.
000143     05 WS-CARD-COUNT         PIC 9(7) VALUE ZERO.
000144     05 WS-TOTAL-COUNT        PIC 9(7) VALUE ZERO.
```

Customer field mapping (lines 271-310):
```cobol
000271         MOVE 'C' TO EXPORT-REC-TYPE
000272         MOVE WS-TIMESTAMP TO EXPORT-TIMESTAMP
000273         ADD 1 TO WS-SEQUENCE-NUM
000274         MOVE WS-SEQUENCE-NUM TO EXPORT-SEQUENCE-NUM
000275         MOVE '0001' TO EXPORT-BRANCH-ID
000276         MOVE 'NORTH' TO EXPORT-REGION-CODE
000277         MOVE CUST-ID TO EXP-CUST-ID
000278         MOVE CUST-FIRST-NAME TO EXP-CUST-FIRST-NAME
000279         MOVE CUST-MIDDLE-NAME TO EXP-CUST-MIDDLE-NAME
000280         MOVE CUST-LAST-NAME TO EXP-CUST-LAST-NAME
000281         MOVE CUST-ADDR-LINE-1 TO EXP-CUST-ADDR-LINE-1
000282         MOVE CUST-ADDR-LINE-2 TO EXP-CUST-ADDR-LINE-2
000283         MOVE CUST-ADDR-LINE-3 TO EXP-CUST-ADDR-LINE-3
000284         MOVE CUST-ADDR-STATE-CD TO EXP-CUST-ADDR-STATE
000285         MOVE CUST-ADDR-COUNTRY-CD TO EXP-CUST-ADDR-COUNTRY
000286         MOVE CUST-ADDR-ZIP TO EXP-CUST-ADDR-ZIP
000287         MOVE CUST-PHONE-NUM-1 TO EXP-CUST-PHONE-1
000288         MOVE CUST-PHONE-NUM-2 TO EXP-CUST-PHONE-2
000289         MOVE CUST-SSN TO EXP-CUST-SSN
000290         MOVE CUST-GOVT-ISSUED-ID TO EXP-CUST-GOVT-ID
000291         MOVE CUST-DOB TO EXP-CUST-DOB
000292         MOVE CUST-EFT-ACCOUNT-ID TO EXP-CUST-EFT-ACCT
000293         MOVE CUST-PRI-CARD-HOLDER-IND TO EXP-CUST-PRI-HOLDER
000294         MOVE CUST-FICO-CREDIT-SCORE TO EXP-CUST-FICO
000310
```

Account field mapping (lines 338-373):
```cobol
000338         MOVE 'A' TO EXPORT-REC-TYPE
000339         MOVE WS-TIMESTAMP TO EXPORT-TIMESTAMP
000340         ADD 1 TO WS-SEQUENCE-NUM
000341         MOVE WS-SEQUENCE-NUM TO EXPORT-SEQUENCE-NUM
000342         MOVE '0001' TO EXPORT-BRANCH-ID
000343         MOVE 'NORTH' TO EXPORT-REGION-CODE
000344         MOVE ACCT-ID TO EXP-ACCT-ID
000345         MOVE ACCT-ACTIVE-STATUS TO EXP-ACCT-STATUS
000346         MOVE ACCT-CURR-BAL TO EXP-ACCT-BALANCE
000347         MOVE ACCT-CREDIT-LIMIT TO EXP-ACCT-CREDIT-LIMIT
000348         MOVE ACCT-CASH-CREDIT-LIMIT TO EXP-ACCT-CASH-LIMIT
000349         MOVE ACCT-OPEN-DATE TO EXP-ACCT-OPEN-DATE
000350         MOVE ACCT-EXPIRAION-DATE TO EXP-ACCT-EXPIRY-DATE
000351         MOVE ACCT-REISSUE-DATE TO EXP-ACCT-REISSUE-DATE
000352         MOVE ACCT-CURR-CYC-CREDIT TO EXP-ACCT-CYC-CREDIT
000353         MOVE ACCT-CURR-CYC-DEBIT TO EXP-ACCT-CYC-DEBIT
000354         MOVE ACCT-ADDR-ZIP TO EXP-ACCT-ZIP
000355         MOVE ACCT-GROUP-ID TO EXP-ACCT-GROUP
000373
```

Transaction field mapping (lines 457-493):
```cobol
000457         MOVE 'T' TO EXPORT-REC-TYPE
000458         MOVE WS-TIMESTAMP TO EXPORT-TIMESTAMP
000459         ADD 1 TO WS-SEQUENCE-NUM
000460         MOVE WS-SEQUENCE-NUM TO EXPORT-SEQUENCE-NUM
000461         MOVE '0001' TO EXPORT-BRANCH-ID
000462         MOVE 'NORTH' TO EXPORT-REGION-CODE
000463         MOVE TRAN-ID TO EXP-TRAN-ID
000464         MOVE TRAN-TYPE-CD TO EXP-TRAN-TYPE
000465         MOVE TRAN-CAT-CD TO EXP-TRAN-CATEGORY
000466         MOVE TRAN-SOURCE TO EXP-TRAN-SOURCE
000467         MOVE TRAN-DESC TO EXP-TRAN-DESC
000468         MOVE TRAN-AMT TO EXP-TRAN-AMOUNT
000469         MOVE TRAN-MERCHANT-ID TO EXP-TRAN-MERCHANT-ID
000470         MOVE TRAN-MERCHANT-NAME TO EXP-TRAN-MERCHANT-NAME
000471         MOVE TRAN-MERCHANT-CITY TO EXP-TRAN-MERCHANT-CITY
000472         MOVE TRAN-MERCHANT-ZIP TO EXP-TRAN-MERCHANT-ZIP
000473         MOVE TRAN-CARD-NUM TO EXP-TRAN-CARD-NUM
000474         MOVE TRAN-ORIG-TS TO EXP-TRAN-ORIG-TS
000475         MOVE TRAN-PROC-TS TO EXP-TRAN-PROC-TS
000493
```

Card field mapping (lines 522-551):
```cobol
000522         MOVE 'D' TO EXPORT-REC-TYPE
000523         MOVE WS-TIMESTAMP TO EXPORT-TIMESTAMP
000524         ADD 1 TO WS-SEQUENCE-NUM
000525         MOVE WS-SEQUENCE-NUM TO EXPORT-SEQUENCE-NUM
000526         MOVE '0001' TO EXPORT-BRANCH-ID
000527         MOVE 'NORTH' TO EXPORT-REGION-CODE
000528         MOVE CARD-NUM TO EXP-CARD-NUM
000529         MOVE CARD-ACCT-ID TO EXP-CARD-ACCT-ID
000530         MOVE CARD-CVV-CD TO EXP-CARD-CVV
000531         MOVE CARD-EMBOSSED-NAME TO EXP-CARD-EMBOSSED-NAME
000532         MOVE CARD-EXPIRAION-DATE TO EXP-CARD-EXPIRY
000533         MOVE CARD-ACTIVE-STATUS TO EXP-CARD-ACTIVE
000551
```

## Acceptance Criteria

### Scenario 1: All record types are exported in correct order

```gherkin
Given the customer, account, cross-reference, transaction, and card VSAM files contain data
When the export program executes
Then customer records are exported first with type indicator 'C'
And account records are exported second with type indicator 'A'
And cross-reference records are exported third with type indicator 'X'
And transaction records are exported fourth with type indicator 'T'
And card records are exported last with type indicator 'D'
```

### Scenario 2: Export records have correct header fields

```gherkin
Given a source customer record exists in the customer file
When the record is exported
Then the export record contains record type 'C'
And a 26-character formatted timestamp
And a unique incrementing sequence number
And branch ID '0001'
And region code 'NORTH'
```

### Scenario 3: Sequence numbers are globally unique and incrementing

```gherkin
Given 10 customer records, 5 account records, and 3 transaction records exist
When all records are exported
Then the sequence numbers range from 1 to 18
And each sequence number is unique across all record types
And the sequence numbers increment monotonically
```

### Scenario 4: Customer PII fields are mapped correctly

```gherkin
Given a customer record with ID 1000001, SSN "123-45-6789", and DOB "1985-03-15"
When the customer record is exported
Then the export record contains the exact SSN value "123-45-6789"
And the export record contains the exact DOB value "1985-03-15"
And all 18 customer fields are mapped to the export layout
```

### Scenario 5: File I/O error triggers abnormal termination

```gherkin
Given the export program is writing records to the export file
When a file I/O error occurs with a non-zero file status
Then the program invokes CEE3ABD for abnormal termination
And no partial export file is left in an inconsistent state
```

### Scenario 6: Export statistics are accurately reported

```gherkin
Given the export program has processed all five input files
When the finalize step executes
Then the displayed customer count matches the number of 'C' records written
And the displayed account count matches the number of 'A' records written
And the displayed total count equals the sum of all individual counts
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5 (data minimization) | Personal data must be adequate, relevant, and limited to what is necessary | The export includes only fields required for branch migration; however, inclusion of SSN, government ID, DOB, and phone numbers must be justified by the migration purpose. The .NET implementation should support field-level filtering |
| GDPR | Art. 44-49 (international transfers) | Personal data transfers outside the EU/EEA require adequate safeguards | The export file contains extensive PII and must not be transferred outside EU data residency boundaries. Branch ID and region code fields must be validated against approved transfer destinations |
| FFFS 2014:5 | Operational continuity | Financial institutions must ensure continuity of critical operations during system changes | The export/import mechanism supports branch migration with full data portability, ensuring operational continuity during infrastructure transitions |
| DORA | Art. 11 | ICT change management procedures must be documented and controlled | The batch export process represents a controlled ICT change procedure with statistics tracking, error handling, and audit trail (timestamps and sequence numbers) |

## Edge Cases

1. **Empty source file**: If any of the five input VSAM files contains no records, the export program should handle the end-of-file condition gracefully, produce zero records for that type, and continue processing the remaining file types.
2. **Maximum sequence number overflow**: The sequence number field (WS-SEQUENCE-NUM) has a defined precision. If the total number of records across all five files exceeds the maximum value (e.g., 9999999 for PIC 9(7)), the sequence number will overflow, causing duplicate keys in the export file.
3. **Timestamp consistency**: All export records share the same timestamp captured during initialization. If the export runs across a midnight boundary, the timestamp will reflect the start time, not the actual export time of each record.
4. **EBCDIC to ASCII conversion**: The source VSAM files use EBCDIC encoding. When migrating to .NET, all character fields must be converted from EBCDIC to Unicode, with special attention to packed decimal (COMP-3) fields like FICO score and account balances.
5. **Hardcoded branch and region**: Branch ID ('0001') and region code ('NORTH') are hardcoded in the COBOL program. The .NET implementation should parameterize these values for multi-branch deployment.
6. **CVV in export**: Card CVV codes are included in the export. PCI-DSS requirements prohibit storage of CVV after authorization; the .NET implementation must evaluate whether CVV should be excluded or masked in export records.
7. **File status check coverage**: Every OPEN, READ, and WRITE operation checks the file status. The .NET implementation must replicate this level of error checking, translating file status codes to appropriate exceptions.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The export includes sensitive PII fields (SSN, government ID, CVV) that raise significant GDPR and PCI-DSS concerns. Confirm whether the branch migration use case justifies including all PII fields, or whether certain fields should be masked or excluded. Also verify whether the hardcoded branch ID '0001' and region code 'NORTH' are specific to one branch or represent default values that should be parameterized.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
