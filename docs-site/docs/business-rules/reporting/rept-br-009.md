---
id: "rept-br-009"
title: "Customer data import with error handling"
domain: "reporting"
cobol_source: "CBIMPORT.cbl:1-488"
requirement_id: "REPT-BR-009"
regulations:
  - "GDPR Art. 5 (data accuracy)"
  - "FFFS 2014:5 (operational risk)"
  - "DORA Art. 11 (ICT change management)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# REPT-BR-009: Customer data import with error handling

## Summary

The CBIMPORT batch program is the counterpart to CBEXPORT (REPT-BR-008). It reads a multi-record export file with 500-byte fixed-length indexed records and splits them into five normalized target files: customers, accounts, cross-references, transactions, and cards. The program processes the export file sequentially, dispatching each record by its type indicator using a COBOL EVALUATE statement. For each known record type ('C', 'A', 'X', 'T', 'D'), the program initializes a fresh target record, maps the export fields to the target layout, and writes the record to the corresponding output file. Unknown record types are captured in an error output file with a 132-byte error record containing a timestamp, record type, sequence number, and descriptive error message. The program tracks comprehensive import statistics including total records read, per-type import counts, error counts, and unknown type counts. All files are properly closed during finalization, and a summary of import statistics is displayed upon completion.

## Business Logic

### Pseudocode

```
FUNCTION ImportCustomerData():
    // Main processing (0000-MAIN-PROCESSING)
    CALL Initialize()
    CALL ProcessExportFile()
    CALL ValidateImport()
    CALL Finalize()

FUNCTION Initialize() [lines 174-193]:
    CAPTURE current date and time
    OPEN INPUT exportFile (indexed, 500-byte records)
    OPEN OUTPUT customerFile, accountFile, xrefFile, transactionFile, cardFile
    OPEN OUTPUT errorFile
    SET totalRead = 0
    SET customerImported = 0, accountImported = 0
    SET xrefImported = 0, transactionImported = 0
    SET cardImported = 0, errorsWritten = 0, unknownTypes = 0

FUNCTION ProcessExportFile() [lines 248-256]:
    READ FIRST record from exportFile
    WHILE NOT end-of-file:
        totalRead = totalRead + 1
        CALL DispatchByRecordType(exportRecord)
        READ NEXT record from exportFile
    END WHILE

FUNCTION DispatchByRecordType(exportRecord) [lines 270-285]:
    EVALUATE exportRecord.RecordType:
        WHEN 'C':
            INITIALIZE customerRecord
            MAP export customer fields -> target customer fields
            WRITE customerRecord TO customerFile
            customerImported = customerImported + 1

        WHEN 'A':
            INITIALIZE accountRecord
            MAP export account fields -> target account fields
            WRITE accountRecord TO accountFile
            accountImported = accountImported + 1

        WHEN 'X':
            INITIALIZE xrefRecord
            MAP export xref fields -> target xref fields
            WRITE xrefRecord TO xrefFile
            xrefImported = xrefImported + 1

        WHEN 'T':
            INITIALIZE transactionRecord
            MAP export transaction fields -> target transaction fields
            WRITE transactionRecord TO transactionFile
            transactionImported = transactionImported + 1

        WHEN 'D':
            INITIALIZE cardRecord
            MAP export card fields -> target card fields
            WRITE cardRecord TO cardFile
            cardImported = cardImported + 1

        WHEN OTHER:
            CALL WriteErrorRecord(exportRecord)
            unknownTypes = unknownTypes + 1
    END EVALUATE

FUNCTION WriteErrorRecord(exportRecord) [lines 425-434]:
    INITIALIZE errorRecord (132 bytes)
    errorRecord.Timestamp = CURRENT_TIMESTAMP (26 chars)
    errorRecord.Separator1 = '|'
    errorRecord.RecordType = exportRecord.RecordType (1 char)
    errorRecord.Separator2 = '|'
    errorRecord.SequenceNum = exportRecord.SequenceNum (7 chars)
    errorRecord.Separator3 = '|'
    errorRecord.Message = "Unknown record type encountered" (50 chars)
    errorRecord.Filler = SPACES (43 chars)
    WRITE errorRecord TO errorFile
    errorsWritten = errorsWritten + 1

FUNCTION ValidateImport() [lines 449-452]:
    DISPLAY "Import processing complete"
    // Placeholder for future validation logic

FUNCTION Finalize() [lines 455-478]:
    CLOSE exportFile
    CLOSE customerFile, accountFile, xrefFile, transactionFile, cardFile
    CLOSE errorFile
    DISPLAY "Import Statistics:"
    DISPLAY "  Total Read:        " + totalRead
    DISPLAY "  Customers:         " + customerImported
    DISPLAY "  Accounts:          " + accountImported
    DISPLAY "  Cross-References:  " + xrefImported
    DISPLAY "  Transactions:      " + transactionImported
    DISPLAY "  Cards:             " + cardImported
    DISPLAY "  Errors:            " + errorsWritten
    DISPLAY "  Unknown Types:     " + unknownTypes
    STOP RUN
END FUNCTION
```

### Decision Table

| Export Record Type | Target File | Action | Counter Incremented |
|--------------------|-------------|--------|---------------------|
| 'C' | CUSTOUT (customer) | INITIALIZE + MAP + WRITE | customerImported |
| 'A' | ACCTOUT (account) | INITIALIZE + MAP + WRITE | accountImported |
| 'X' | XREFOUT (cross-reference) | INITIALIZE + MAP + WRITE | xrefImported |
| 'T' | TRNXOUT (transaction) | INITIALIZE + MAP + WRITE | transactionImported |
| 'D' | CARDOUT (card) | INITIALIZE + MAP + WRITE | cardImported |
| OTHER | ERROUT (error) | Write timestamped error record | errorsWritten, unknownTypes |

## Source COBOL Reference

**Program:** `CBIMPORT.cbl`
**Lines:** 1-488 (Complete batch import program)

Main processing control flow (lines 165-171):
```cobol
000165     PERFORM 0000-INITIALIZE
000166     PERFORM 1000-PROCESS-EXPORT-FILE
000167     PERFORM 8000-VALIDATE-IMPORT
000168     PERFORM 9000-FINALIZE
000169     STOP RUN
000170     .
000171
```

Initialize - open files and capture timestamp (lines 174-193):
```cobol
000174 0000-INITIALIZE.
000175     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
000176     MOVE WS-CURRENT-DATE TO WS-TIMESTAMP
000177     OPEN INPUT EXPORT-FILE
000178     IF WS-EXP-FILE-STATUS NOT = '00'
000179         DISPLAY 'ERROR OPENING EXPORT FILE: ' WS-EXP-FILE-STATUS
000180         CALL 'CEE3ABD' USING WS-ABEND-CODE WS-ABEND-TIMING
000181     END-IF
000182     OPEN OUTPUT CUST-OUT-FILE
000183     OPEN OUTPUT ACCT-OUT-FILE
000184     OPEN OUTPUT XREF-OUT-FILE
000185     OPEN OUTPUT TRAN-OUT-FILE
000186     OPEN OUTPUT CARD-OUT-FILE
000187     OPEN OUTPUT ERROR-OUT-FILE
000188     INITIALIZE WS-IMPORT-STATS
000193     .
```

Process export file - sequential read loop (lines 248-256):
```cobol
000248 1000-PROCESS-EXPORT-FILE.
000249     READ EXPORT-FILE INTO WS-EXPORT-RECORD
000250     PERFORM UNTIL WS-EXP-FILE-STATUS = '10'
000251         ADD 1 TO WS-TOTAL-READ
000252         PERFORM 2000-DISPATCH-BY-TYPE
000253         READ EXPORT-FILE INTO WS-EXPORT-RECORD
000254     END-PERFORM
000255     .
000256
```

Record type dispatch (lines 270-285):
```cobol
000270 2000-DISPATCH-BY-TYPE.
000271     EVALUATE EXPORT-REC-TYPE
000272         WHEN 'C'
000273             PERFORM 3000-IMPORT-CUSTOMER
000274         WHEN 'A'
000275             PERFORM 4000-IMPORT-ACCOUNT
000276         WHEN 'X'
000277             PERFORM 5000-IMPORT-XREF
000278         WHEN 'T'
000279             PERFORM 6000-IMPORT-TRANSACTION
000280         WHEN 'D'
000281             PERFORM 7000-IMPORT-CARD
000282         WHEN OTHER
000283             PERFORM 7500-WRITE-ERROR-RECORD
000284     END-EVALUATE
000285     .
```

Error record writing (lines 425-434):
```cobol
000425 7500-WRITE-ERROR-RECORD.
000426     INITIALIZE WS-ERROR-RECORD
000427     MOVE WS-TIMESTAMP TO ERR-TIMESTAMP
000428     MOVE '|' TO ERR-SEP-1
000429     MOVE EXPORT-REC-TYPE TO ERR-REC-TYPE
000430     MOVE '|' TO ERR-SEP-2
000431     MOVE EXPORT-SEQUENCE-NUM TO ERR-SEQ-NUM
000432     MOVE '|' TO ERR-SEP-3
000433     MOVE 'Unknown record type encountered' TO ERR-MESSAGE
000434     WRITE ERROR-RECORD FROM WS-ERROR-RECORD
```

Error record layout (lines 152-160):
```cobol
000152 01  WS-ERROR-RECORD.
000153     05 ERR-TIMESTAMP        PIC X(26).
000154     05 ERR-SEP-1            PIC X(1).
000155     05 ERR-REC-TYPE         PIC X(1).
000156     05 ERR-SEP-2            PIC X(1).
000157     05 ERR-SEQ-NUM          PIC 9(7).
000158     05 ERR-SEP-3            PIC X(1).
000159     05 ERR-MESSAGE          PIC X(50).
000160     05 ERR-FILLER           PIC X(43).
```

Finalize - close files and display statistics (lines 455-478):
```cobol
000455 9000-FINALIZE.
000456     CLOSE EXPORT-FILE
000457     CLOSE CUST-OUT-FILE
000458     CLOSE ACCT-OUT-FILE
000459     CLOSE XREF-OUT-FILE
000460     CLOSE TRAN-OUT-FILE
000461     CLOSE CARD-OUT-FILE
000462     CLOSE ERROR-OUT-FILE
000463     DISPLAY 'IMPORT STATISTICS:'
000464     DISPLAY '  TOTAL READ:        ' WS-TOTAL-READ
000465     DISPLAY '  CUSTOMERS:         ' WS-CUST-IMPORTED
000466     DISPLAY '  ACCOUNTS:          ' WS-ACCT-IMPORTED
000467     DISPLAY '  CROSS-REFERENCES:  ' WS-XREF-IMPORTED
000468     DISPLAY '  TRANSACTIONS:      ' WS-TRAN-IMPORTED
000469     DISPLAY '  CARDS:             ' WS-CARD-IMPORTED
000470     DISPLAY '  ERRORS:            ' WS-ERRORS-WRITTEN
000471     DISPLAY '  UNKNOWN TYPES:     ' WS-UNKNOWN-TYPES
000478     .
```

## Acceptance Criteria

### Scenario 1: Customer records are imported correctly

```gherkin
Given the export file contains records with type indicator 'C'
When the import program processes each 'C' record
Then a new customer record is initialized
And all customer fields are mapped from the export layout to the target layout
And the customer record is written to the customer output file
And the customer import counter is incremented
```

### Scenario 2: All five record types are dispatched correctly

```gherkin
Given the export file contains records of types 'C', 'A', 'X', 'T', and 'D'
When the import program reads each record
Then 'C' records are written to the customer output file
And 'A' records are written to the account output file
And 'X' records are written to the cross-reference output file
And 'T' records are written to the transaction output file
And 'D' records are written to the card output file
```

### Scenario 3: Unknown record types generate error records

```gherkin
Given the export file contains a record with type indicator 'Z'
When the import program processes this record
Then a 132-byte error record is written to the error output file
And the error record contains a 26-character timestamp
And the error record contains the unknown type 'Z' separated by pipe delimiters
And the error record contains the sequence number of the offending record
And the error message reads "Unknown record type encountered"
And the unknown type counter is incremented
```

### Scenario 4: Import statistics are accurate and comprehensive

```gherkin
Given the export file contains 100 customer, 50 account, 200 cross-reference, 500 transaction, 75 card, and 3 unknown records
When the import program completes processing
Then the total read count displays 928
And the customer imported count displays 100
And the account imported count displays 50
And the cross-reference imported count displays 200
And the transaction imported count displays 500
And the card imported count displays 75
And the errors written count displays 3
And the unknown types count displays 3
```

### Scenario 5: Export file open failure triggers abnormal termination

```gherkin
Given the export file does not exist or is inaccessible
When the import program attempts to open the export file
Then the file status is not '00'
And an error message is displayed with the file status code
And the program invokes CEE3ABD for abnormal termination
```

### Scenario 6: Target records are initialized before field mapping

```gherkin
Given a customer export record is being processed
When the import program creates the target customer record
Then the target record is initialized with default values (INITIALIZE)
And then the export fields are mapped to the target record
And no residual data from previous records contaminates the new record
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5 (data accuracy) | Personal data must be accurate and kept up to date | The INITIALIZE before mapping ensures no residual data contamination between records. Error handling captures unknown record types rather than silently discarding them, maintaining data completeness and accuracy |
| FFFS 2014:5 | Operational risk management | Financial institutions must manage operational risks including data processing errors | Comprehensive error handling with timestamped error records, per-type statistics tracking, and abnormal termination on file errors ensures operational failures are detected and recorded rather than silently ignored |
| DORA | Art. 11 | ICT change management must include testing and validation procedures | The ValidateImport placeholder indicates a designed validation checkpoint. The import statistics provide an audit trail for reconciliation between export and import counts, supporting change management verification |

## Edge Cases

1. **Empty export file**: If the export file contains no records, the sequential read loop terminates immediately on the first READ returning file status '10'. All counters remain at zero, and the program completes normally with zero statistics.
2. **Corrupted record type field**: If a record's type indicator is corrupted (e.g., contains a control character or binary data), it falls through to the OTHER case and is written to the error file. The error record preserves the corrupt byte for diagnosis.
3. **Error file I/O failure**: If writing to the error output file fails, the program may not have explicit error handling for the error file itself, potentially causing an unhandled abend. The .NET implementation should handle error-during-error-handling gracefully.
4. **Sequence number in error record**: The sequence number is formatted as PIC 9(7), supporting up to 9,999,999. If the export file contains records with sequence numbers exceeding this, the error record will truncate the sequence number.
5. **Record order dependency**: The import program processes records in the order they appear in the export file. If cross-reference records reference customer or account IDs not yet imported (due to ordering), referential integrity cannot be validated during import.
6. **ValidateImport placeholder**: The validation step (lines 449-452) is currently a placeholder that only displays a completion message. The .NET implementation should add actual validation logic such as cross-file referential integrity checks and count reconciliation.
7. **Pipe-delimited error format**: The error record uses pipe ('|') as a field separator in a fixed-width layout. If any source field contains a pipe character, it could confuse downstream error parsing tools.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The ValidateImport section is a placeholder in the COBOL source (lines 449-452). Confirm whether validation logic was planned but never implemented, or whether validation occurs in a separate downstream process. The .NET implementation should include explicit reconciliation checks: total records read equals sum of all type counts plus error counts, and cross-file referential integrity between customers, accounts, and cross-references.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
