---
id: "acct-br-007"
title: "Batch account data extract and multi-format export"
domain: "account-management"
cobol_source: "CBACT01C.cbl:1-431"
requirement_id: "ACCT-BR-007"
regulations:
  - "FSA FFFS 2014:5 Ch. 8"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# ACCT-BR-007: Batch account data extract and multi-format export

## Summary

CBACT01C is a batch COBOL program that reads the account master VSAM file (ACCTDAT) sequentially and produces three different output formats for downstream processing. The program opens the input account file and three output files, reads every account record in sequence, and for each record writes one record to each output file in a distinct format. This is a critical data extract and reporting program used for reconciliation, downstream system feeds, and parallel-run validation. The three output formats are: (1) a sequential flat file with field-by-field copy and date reformatting, (2) an array-format file with a 5-occurrence repeating structure containing hardcoded test values, and (3) a variable-length record file emitting two record types per account. Notable business logic includes a default value substitution (2525.00) when the current cycle debit is zero, and date reformatting via an external assembler routine (COBDATFT). All file operations are status-checked with ABEND on unexpected conditions.

## Business Logic

### Pseudocode

```
PROGRAM CBACT01C:

    OPEN INPUT  ACCTDAT-FILE    (VSAM KSDS, account master)
    OPEN OUTPUT OUTFILE         (sequential flat file)
    OPEN OUTPUT ARRYFILE        (array-format file)
    OPEN OUTPUT VBRCFILE        (variable-length record file)

    PERFORM 1000-READ-ACCTDAT
    PERFORM UNTIL end-of-file
        DISPLAY account-record

        -- Output 1: Sequential flat file (OUTFILE)
        MOVE ACCT-ID              TO OUT-ACCT-ID
        MOVE ACCT-ACTIVE-STATUS   TO OUT-ACTIVE-STATUS
        MOVE ACCT-CURR-BAL        TO OUT-CURR-BAL
        MOVE ACCT-CREDIT-LIMIT    TO OUT-CREDIT-LIMIT
        MOVE ACCT-CASH-CREDIT-LIMIT TO OUT-CASH-CREDIT-LIMIT
        MOVE ACCT-OPEN-DATE       TO OUT-OPEN-DATE
        MOVE ACCT-EXPIRAION-DATE  TO OUT-EXPIRAION-DATE
        -- Reformat reissue date via COBDATFT assembler routine
        MOVE '2' TO CODATECN-TYPE-IN
        MOVE '2' TO CODATECN-TYPE-OUT
        MOVE ACCT-REISSUE-DATE TO CODATECN-DATE-IN
        CALL 'COBDATFT' USING CODATECN-REC
        MOVE CODATECN-DATE-OUT TO OUT-REISSUE-DATE
        MOVE ACCT-CURR-CYC-CREDIT TO OUT-CURR-CYC-CREDIT
        -- Default value substitution for zero debit
        IF ACCT-CURR-CYC-DEBIT = ZERO
            MOVE 2525.00 TO OUT-CURR-CYC-DEBIT
        ELSE
            MOVE ACCT-CURR-CYC-DEBIT TO OUT-CURR-CYC-DEBIT
        END-IF
        WRITE OUTFILE-REC

        -- Output 2: Array-format file (ARRYFILE)
        MOVE ACCT-ID TO ARRY-ACCT-ID
        -- Occurrence 1: actual balance + hardcoded debit
        MOVE ACCT-CURR-BAL TO ARRY-CURR-BAL(1)
        MOVE 1005.00       TO ARRY-CURR-DEBIT(1)
        -- Occurrence 2: actual balance + hardcoded debit
        MOVE ACCT-CURR-BAL TO ARRY-CURR-BAL(2)
        MOVE 1525.00       TO ARRY-CURR-DEBIT(2)
        -- Occurrence 3: hardcoded values
        MOVE -1025.00      TO ARRY-CURR-BAL(3)
        MOVE -2500.00      TO ARRY-CURR-DEBIT(3)
        WRITE ARRYFILE-REC

        -- Output 3: Variable-length record file (VBRCFILE)
        -- Type 1 record (12 bytes): Account ID + Active Status
        MOVE ACCT-ID            TO VBRC-ACCT-ID-1
        MOVE ACCT-ACTIVE-STATUS TO VBRC-ACTIVE-STATUS
        SET VBRC-REC-LENGTH TO 12
        WRITE VBRCFILE-REC

        -- Type 2 record (39 bytes): Account ID + Balance + Credit Limit + Reissue Year
        MOVE ACCT-ID           TO VBRC-ACCT-ID-2
        MOVE ACCT-CURR-BAL     TO VBRC-CURR-BAL
        MOVE ACCT-CREDIT-LIMIT TO VBRC-CREDIT-LIMIT
        MOVE ACCT-REISSUE-DATE(1:4) TO VBRC-REISSUE-YEAR
        SET VBRC-REC-LENGTH TO 39
        WRITE VBRCFILE-REC

        PERFORM 1000-READ-ACCTDAT
    END-PERFORM

    CLOSE ACCTDAT-FILE, OUTFILE, ARRYFILE, VBRCFILE
    STOP RUN.

1000-READ-ACCTDAT:
    READ ACCTDAT-FILE INTO ACCOUNT-RECORD
    EVALUATE FILE-STATUS
        WHEN '00'   CONTINUE           (success)
        WHEN '10'   SET end-of-file = TRUE   (EOF)
        WHEN OTHER  CALL CEE3ABD USING 999  (ABEND)
    END-EVALUATE.
```

### Decision Table

| ACCT-CURR-CYC-DEBIT Value | OUTFILE Debit Output | Rationale |
|----------------------------|---------------------|-----------|
| Zero (0.00)                | 2525.00 (default)   | Default substitution for zero-debit accounts (line 237) |
| Non-zero (any value)       | Actual value        | Direct copy of account field |

### File Status Decision Table

| File Status Code | Meaning | Action |
|-----------------|---------|--------|
| '00'            | Successful operation | Continue processing |
| '10'            | End of file reached  | Set EOF flag, exit read loop |
| Any other       | Unexpected error     | ABEND with code 999 via CEE3ABD |

## Source COBOL Reference

**Program:** `CBACT01C.cbl`
**Lines:** 1-431 (complete program)

**Key sections:**
- File definitions: lines 28-85
- Main processing loop: lines 147-154
- Sequential output population with default debit: lines 215-240
- Array output population: lines 253-261
- Variable-length record output: lines 276-315
- File status error handling: lines 400-431

```cobol
000028 FD  ACCTDAT-FILE
000029     RECORD CONTAINS 300 CHARACTERS.
000030 01  FD-ACCTDAT-REC              PIC X(300).
```

```cobol
000089     COPY CVACT01Y.
```

```cobol
000147     PERFORM 1000-ACCTDAT-READ.
000148     PERFORM UNTIL ACCTDAT-EOF
000149         DISPLAY ACCOUNT-RECORD
000150         PERFORM 1100-POPULATE-OUTFILE
000151         PERFORM 1200-POPULATE-ARRYFILE
000152         PERFORM 1300-POPULATE-VBRCFILE
000153         PERFORM 1000-ACCTDAT-READ
000154     END-PERFORM.
```

```cobol
000231     CALL 'COBDATFT' USING CODATECN-REC.
000232     MOVE CODATECN-DATE-OUT TO OUT-REISSUE-DATE.
```

```cobol
000237     IF ACCT-CURR-CYC-DEBIT EQUAL TO ZERO
000238         MOVE 2525.00 TO OUT-CURR-CYC-DEBIT
000239     ELSE
000240         MOVE ACCT-CURR-CYC-DEBIT TO OUT-CURR-CYC-DEBIT.
```

```cobol
000253     MOVE ACCT-CURR-BAL   TO ARRY-CURR-BAL(1).
000254     MOVE 1005.00         TO ARRY-CURR-DEBIT(1).
000255     MOVE ACCT-CURR-BAL   TO ARRY-CURR-BAL(2).
000256     MOVE 1525.00         TO ARRY-CURR-DEBIT(2).
000257     MOVE -1025.00        TO ARRY-CURR-BAL(3).
000258     MOVE -2500.00        TO ARRY-CURR-DEBIT(3).
```

```cobol
000276     MOVE ACCT-ID            TO VBRC-ACCT-ID-1.
000277     MOVE ACCT-ACTIVE-STATUS TO VBRC-ACTIVE-STATUS.
```

```cobol
000410     IF ACCTDAT-STATUS NOT = '00'
000411       AND ACCTDAT-STATUS NOT = '10'
000412         CALL 'CEE3ABD' USING 999
000413     END-IF.
```

**File definitions and record layouts:**

| File | DD Name | Access | Record Length | Description |
|------|---------|--------|---------------|-------------|
| ACCTDAT | ACCTDAT-FILE | Sequential read (input) | 300 bytes (fixed) | Account master VSAM KSDS |
| OUTFILE | OUTFILE | Sequential write (output) | Fixed | Flat file field-by-field extract |
| ARRYFILE | ARRYFILE | Sequential write (output) | Fixed | Array-format with 5 occurrences |
| VBRCFILE | VBRCFILE | Sequential write (output) | Variable (12 or 39 bytes) | Two record types per account |

## Acceptance Criteria

### Scenario 1: Sequential flat file output contains all account fields

```gherkin
GIVEN the ACCTDAT file contains account records
WHEN the CBACT01C batch program is executed
THEN the OUTFILE contains one record for each account in the input
  AND each output record contains ACCT-ID, ACCT-ACTIVE-STATUS, ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, reformatted ACCT-REISSUE-DATE, ACCT-CURR-CYC-CREDIT, and ACCT-CURR-CYC-DEBIT
```

### Scenario 2: Default debit substitution when current cycle debit is zero

```gherkin
GIVEN an account record with ACCT-CURR-CYC-DEBIT equal to zero
WHEN the record is written to OUTFILE
THEN the output debit field contains 2525.00
  AND the original account record is not modified
```

### Scenario 3: Actual debit preserved when current cycle debit is non-zero

```gherkin
GIVEN an account record with ACCT-CURR-CYC-DEBIT equal to 150.75
WHEN the record is written to OUTFILE
THEN the output debit field contains 150.75
```

### Scenario 4: Reissue date is reformatted via COBDATFT

```gherkin
GIVEN an account record with a populated ACCT-REISSUE-DATE
WHEN the record is processed for OUTFILE output
THEN the COBDATFT routine is called with type code '2' for both input and output format
  AND the reformatted date is written to OUT-REISSUE-DATE
```

### Scenario 5: Array-format output with hardcoded test values

```gherkin
GIVEN an account record with ACCT-CURR-BAL = 5000.00
WHEN the record is written to ARRYFILE
THEN occurrence 1 contains balance 5000.00 and debit 1005.00
  AND occurrence 2 contains balance 5000.00 and debit 1525.00
  AND occurrence 3 contains balance -1025.00 and debit -2500.00
```

### Scenario 6: Variable-length file produces two records per account

```gherkin
GIVEN an account record with ACCT-ID = "00012345678" and ACCT-ACTIVE-STATUS = "Y"
  AND ACCT-CURR-BAL = 3000.00 and ACCT-CREDIT-LIMIT = 10000.00
  AND ACCT-REISSUE-DATE starting with year "2025"
WHEN the record is written to VBRCFILE
THEN a Type 1 record of 12 bytes is written containing the account ID and active status
  AND a Type 2 record of 39 bytes is written containing the account ID, current balance, credit limit, and reissue year
```

### Scenario 7: File status error causes ABEND

```gherkin
GIVEN the ACCTDAT file encounters an unexpected file status (not '00' or '10')
WHEN the file status is checked
THEN the program ABENDs with code 999 via CEE3ABD
  AND no partial output records are written after the error
```

### Scenario 8: Empty input file produces no output

```gherkin
GIVEN the ACCTDAT file contains zero records
WHEN the CBACT01C batch program is executed
THEN all three output files are opened and closed cleanly
  AND no records are written to OUTFILE, ARRYFILE, or VBRCFILE
  AND the program terminates with a normal return code
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 8 | Credit institutions must maintain reliable reporting and data export capabilities for supervisory purposes | The batch extract produces structured output files for downstream regulatory reporting and reconciliation; the migrated system must produce identical output for parallel-run validation |
| DORA | Art. 11 | ICT systems must support backup, restoration, and recovery including data integrity verification | The multi-format extract serves as a data integrity verification mechanism by producing redundant output formats that can be cross-checked; the migrated system must replicate this capability for operational resilience testing |

## Edge Cases

1. **Default debit value of 2525.00 for zero-balance accounts**: The substitution of 2525.00 when ACCT-CURR-CYC-DEBIT is zero appears to be a legacy business convention or a test data artifact. The migrated system must replicate this exact behavior during parallel-run, but the business justification should be confirmed by domain experts. If this is test data generation logic, the migrated system may choose to make this value configurable.

2. **Array file hardcoded values are likely test data**: The array-format output uses hardcoded values (1005.00, 1525.00, -1025.00, -2500.00) for occurrences 1-3, with occurrences 4 and 5 unused. This strongly suggests the array output is used for test/sample data generation rather than production reporting. The migrated system must still replicate this behavior for parallel-run comparison, but the purpose should be documented and confirmed.

3. **COBDATFT assembler routine dependency**: The date reformatting relies on an external assembler routine (COBDATFT) with type code '2' for both input and output formats. The migrated system must understand what date transformation this performs (likely between internal and display formats). If the routine is unavailable for analysis, date conversion behavior must be reverse-engineered from sample data.

4. **Variable-length record file format**: The VBRCFILE uses variable-length records (12 bytes and 39 bytes). The migrated system must handle the record descriptor word (RDW) that precedes each variable-length record in z/OS datasets. When producing equivalent output on Azure, the format convention (CSV, JSON, fixed-length with type indicator) must be agreed upon.

5. **Console DISPLAY for every record**: The program DISPLAYs every account record to the console (SYSOUT). For large account files, this produces extensive log output. The migrated system should implement equivalent logging at an appropriate verbosity level (e.g., DEBUG) rather than writing every record to standard output.

6. **No record count or checksum**: The program does not produce a control total or record count at the end of processing. The migrated system should consider adding record counts and hash totals for reconciliation, especially during parallel-run where output comparison is critical.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions for the retiring COBOL developers: (1) What is the business purpose of the 2525.00 default debit substitution -- is this a regulatory reporting convention or a test data artifact? (2) Is the array-format output (ARRYFILE) used in production, or is it solely for testing? (3) What specific date transformation does the COBDATFT routine perform with type code '2'? (4) Which downstream systems consume each of the three output files? (5) Are there any JCL-level record count verification steps (e.g., IDCAMS VERIFY) that run after this program?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
