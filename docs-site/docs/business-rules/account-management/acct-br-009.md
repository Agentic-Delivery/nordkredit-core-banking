---
id: "acct-br-009"
title: "Batch card data and cross-reference reporting"
domain: "account-management"
cobol_source: "CBACT02C.cbl:1-179"
requirement_id: "ACCT-BR-009"
regulations:
  - "FSA FFFS 2014:5 Ch. 8"
status: "extracted"
validated_by: null
validated_date: null
priority: "low"
---

# ACCT-BR-009: Batch card data and cross-reference reporting

## Summary

CBACT02C and CBACT03C are simple batch reporting programs that read and display card data and cross-reference data respectively. They follow identical structural patterns: open a VSAM KSDS file, read records sequentially in a loop, DISPLAY each record to SYSOUT, and close the file on EOF. CBACT02C reads the card data file (CARDFILE) using the CVACT02Y copybook for the CARD-RECORD structure, while CBACT03C reads the cross-reference file (XREFFILE) using the CVACT03Y copybook for the CARD-XREF-RECORD structure. Both programs enforce strict file status checking: '00' for successful operations, '10' for end-of-file, and any other status triggers an ABEND via CEE3ABD with code 999. These programs provide audit trail and reconciliation capability by dumping complete file contents, which is essential during parallel-run validation to verify data integrity between the mainframe and the migrated system.

## Business Logic

### Pseudocode

```
PROGRAM CBACT02C (Card Data Report):

    OPEN INPUT CARDFILE       (VSAM KSDS, key: FD-CARD-NUM PIC X(16))

    PERFORM 1000-READ-CARDFILE
    PERFORM UNTIL end-of-file
        DISPLAY CARD-RECORD
        PERFORM 1000-READ-CARDFILE
    END-PERFORM

    CLOSE CARDFILE
    STOP RUN.

1000-READ-CARDFILE:
    READ CARDFILE INTO CARD-RECORD
    EVALUATE CARDFILE-STATUS
        WHEN '00'   CONTINUE               (success)
        WHEN '10'   SET end-of-file = TRUE  (EOF)
        WHEN OTHER  CALL CEE3ABD USING 999  (ABEND)
    END-EVALUATE.


PROGRAM CBACT03C (Cross-Reference Report):

    OPEN INPUT XREFFILE       (VSAM KSDS, key: FD-XREF-CARD-NUM PIC X(16))

    PERFORM 1000-READ-XREFFILE
    PERFORM UNTIL end-of-file
        DISPLAY CARD-XREF-RECORD
        PERFORM 1000-READ-XREFFILE
    END-PERFORM

    CLOSE XREFFILE
    STOP RUN.

1000-READ-XREFFILE:
    READ XREFFILE INTO CARD-XREF-RECORD
    EVALUATE XREFFILE-STATUS
        WHEN '00'   CONTINUE               (success)
        WHEN '10'   SET end-of-file = TRUE  (EOF)
        WHEN OTHER  CALL CEE3ABD USING 999  (ABEND)
    END-EVALUATE.
```

### Data Structure: CARD-RECORD (from CVACT02Y.cpy)

```
CARD-RECORD:
    CARD-NUM              PIC X(16)    -- 16-digit card number (primary key)
    CARD-ACCT-ID          PIC 9(11)    -- Associated account ID
    CARD-CVV-CD           PIC 9(03)    -- Card CVV code
    CARD-EMBOSSED-NAME    PIC X(50)    -- Name embossed on card
    CARD-EXPIRAION-DATE   PIC X(10)    -- Expiry date YYYY-MM-DD (misspelling preserved)
    CARD-ACTIVE-STATUS    PIC X(01)    -- Active status: 'Y' = active, 'N' = inactive
```

### Data Structure: CARD-XREF-RECORD (from CVACT03Y.cpy)

```
CARD-XREF-RECORD:
    XREF-CARD-NUM         PIC X(16)    -- Card number (primary key)
    XREF-CUST-ID          PIC 9(09)    -- Customer ID
    XREF-ACCT-ID          PIC 9(11)    -- Account ID
    XREF-FILLER           PIC X(14)    -- Reserved space
```

### Processing Flow (both programs)

```
1. OPEN INPUT file
2. READ first record
3. LOOP:
   a. IF file-status = '10' (EOF) -> EXIT LOOP
   b. IF file-status != '00' -> ABEND 999
   c. DISPLAY record to SYSOUT
   d. READ next record
4. CLOSE file
5. STOP RUN
```

## Source COBOL Reference

**Program:** `CBACT02C.cbl`
**Lines:** 1-179

```cobol
000028 FD  CARDFILE
000029     RECORD CONTAINS 150 CHARACTERS.
000030 01  FD-CARD-REC.
000031     05  FD-CARD-NUM            PIC X(16).
000032     05  FILLER                 PIC X(134).
```

```cobol
000055     COPY CVACT02Y.
```

```cobol
000100     OPEN INPUT CARDFILE.
000101     IF CARDFILE-STATUS NOT = '00'
000102         DISPLAY 'ERROR OPENING CARDFILE'
000103         CALL 'CEE3ABD' USING 999
000104     END-IF.
```

```cobol
000120     PERFORM 1000-READ-CARDFILE.
000121     PERFORM UNTIL CARDFILE-EOF
000122         DISPLAY CARD-RECORD
000123         PERFORM 1000-READ-CARDFILE
000124     END-PERFORM.
```

```cobol
000150 1000-READ-CARDFILE.
000151     READ CARDFILE INTO CARD-RECORD.
000152     EVALUATE CARDFILE-STATUS
000153         WHEN '00'
000154             CONTINUE
000155         WHEN '10'
000156             SET CARDFILE-EOF TO TRUE
000157         WHEN OTHER
000158             DISPLAY 'ERROR READING CARDFILE: ' CARDFILE-STATUS
000159             CALL 'CEE3ABD' USING 999
000160     END-EVALUATE.
```

**Program:** `CBACT03C.cbl`
**Lines:** 1-179

```cobol
000028 FD  XREFFILE
000029     RECORD CONTAINS 50 CHARACTERS.
000030 01  FD-XREF-REC.
000031     05  FD-XREF-CARD-NUM       PIC X(16).
000032     05  FILLER                 PIC X(34).
```

```cobol
000055     COPY CVACT03Y.
```

```cobol
000100     OPEN INPUT XREFFILE.
000101     IF XREFFILE-STATUS NOT = '00'
000102         DISPLAY 'ERROR OPENING XREFFILE'
000103         CALL 'CEE3ABD' USING 999
000104     END-IF.
```

```cobol
000120     PERFORM 1000-READ-XREFFILE.
000121     PERFORM UNTIL XREFFILE-EOF
000122         DISPLAY CARD-XREF-RECORD
000123         PERFORM 1000-READ-XREFFILE
000124     END-PERFORM.
```

```cobol
000150 1000-READ-XREFFILE.
000151     READ XREFFILE INTO CARD-XREF-RECORD.
000152     EVALUATE XREFFILE-STATUS
000153         WHEN '00'
000154             CONTINUE
000155         WHEN '10'
000156             SET XREFFILE-EOF TO TRUE
000157         WHEN OTHER
000158             DISPLAY 'ERROR READING XREFFILE: ' XREFFILE-STATUS
000159             CALL 'CEE3ABD' USING 999
000160     END-EVALUATE.
```

**Copybook data structures:**

| Copybook | Record | Key Field | Record Length | Used By |
|----------|--------|-----------|---------------|---------|
| CVACT02Y | CARD-RECORD | CARD-NUM PIC X(16) | 150 bytes | CBACT02C |
| CVACT03Y | CARD-XREF-RECORD | XREF-CARD-NUM PIC X(16) | 50 bytes | CBACT03C |

## Acceptance Criteria

### Scenario 1: Card data report outputs all card records

```gherkin
GIVEN the CARDFILE contains 100 card records
WHEN the CBACT02C batch program is executed
THEN all 100 card records are displayed to SYSOUT
  AND each record contains CARD-NUM, CARD-ACCT-ID, CARD-CVV-CD, CARD-EMBOSSED-NAME, CARD-EXPIRAION-DATE, and CARD-ACTIVE-STATUS
  AND the program terminates normally after reaching end-of-file
```

### Scenario 2: Cross-reference report outputs all cross-reference records

```gherkin
GIVEN the XREFFILE contains 250 cross-reference records
WHEN the CBACT03C batch program is executed
THEN all 250 cross-reference records are displayed to SYSOUT
  AND each record contains XREF-CARD-NUM, XREF-CUST-ID, and XREF-ACCT-ID
  AND the program terminates normally after reaching end-of-file
```

### Scenario 3: Empty file produces no output

```gherkin
GIVEN the CARDFILE contains zero records
WHEN the CBACT02C batch program is executed
THEN no records are displayed to SYSOUT
  AND the program terminates normally (no ABEND)
```

### Scenario 4: File open error triggers ABEND

```gherkin
GIVEN the CARDFILE cannot be opened (file not found or access denied)
WHEN the CBACT02C batch program attempts to open the file
THEN the error message 'ERROR OPENING CARDFILE' is displayed
  AND the program ABENDs with code 999 via CEE3ABD
```

### Scenario 5: File read error triggers ABEND

```gherkin
GIVEN the XREFFILE is opened successfully
  AND a read operation returns an unexpected file status (not '00' or '10')
WHEN the file status is evaluated
THEN the error message 'ERROR READING XREFFILE: ' followed by the status code is displayed
  AND the program ABENDs with code 999 via CEE3ABD
```

### Scenario 6: Card record data integrity for parallel-run comparison

```gherkin
GIVEN the CARDFILE contains a card record with:
  | Field               | Value              |
  | CARD-NUM            | 4000123456789012   |
  | CARD-ACCT-ID        | 00012345678        |
  | CARD-CVV-CD         | 123                |
  | CARD-EMBOSSED-NAME  | ERIK SVENSSON      |
  | CARD-EXPIRAION-DATE | 2027-06-30         |
  | CARD-ACTIVE-STATUS  | Y                  |
WHEN the record is displayed by CBACT02C
THEN the migrated system produces identical output for the same input record
  AND EBCDIC-to-Unicode conversion does not alter any field values
```

### Scenario 7: Cross-reference record data integrity for parallel-run comparison

```gherkin
GIVEN the XREFFILE contains a cross-reference record with:
  | Field          | Value            |
  | XREF-CARD-NUM  | 4000123456789012 |
  | XREF-CUST-ID   | 000123456        |
  | XREF-ACCT-ID   | 00012345678      |
WHEN the record is displayed by CBACT03C
THEN the migrated system produces identical output for the same input record
  AND the 14-byte FILLER field is not included in the output comparison
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 8 | Credit institutions must maintain adequate systems for record-keeping and be able to produce complete records for supervisory review | The batch reporting programs provide full file dump capability for the card data and cross-reference files, supporting regulatory audits and data integrity verification during and after migration; the migrated system must provide equivalent reporting capability |

## Edge Cases

1. **CVV code in card record output (GDPR/PCI-DSS concern)**: CBACT02C displays the full card record including CARD-CVV-CD (3-digit CVV). In the COBOL batch environment, SYSOUT goes to JES2 spool which is typically access-controlled. The migrated system must ensure that CVV data is not exposed in logs, reports, or console output that could be accessed by unauthorized personnel. PCI-DSS requires CVV to never be stored after authorization; this reporting program may need to mask the CVV field in the migrated version.

2. **Full card number in DISPLAY output**: Both programs output full 16-digit card numbers to SYSOUT. PCI-DSS requires masking of card numbers in logs and displays (show only last 4 digits). The migrated system should mask card numbers in report output unless the report consumer requires full card numbers and appropriate PCI-DSS controls are in place.

3. **Large file performance**: For files with millions of records, DISPLAY to SYSOUT produces extensive output. The migrated system should implement pagination, streaming, or file-based output rather than console display for large datasets. Consider adding a record count parameter to limit output or a sampling mode for quick verification.

4. **FILLER bytes in cross-reference record**: The XREF-FILLER PIC X(14) field is reserved space. If undocumented fields have been placed in this filler region by later program modifications, the DISPLAY will show them but they will not be mapped to named fields. The migration team should sample actual XREFFILE data to check for non-space content in the filler region.

5. **EBCDIC special characters in CARD-EMBOSSED-NAME**: The 50-byte embossed name field may contain EBCDIC characters that do not have direct Unicode equivalents (e.g., accented characters in Swedish names such as a-ring, a-umlaut, o-umlaut). The EBCDIC-to-Unicode conversion must use the correct EBCDIC code page (likely IBM-1047 or IBM-278 for Swedish) to preserve name accuracy.

6. **File status error codes are opaque**: When a non-00/non-10 status occurs, the COBOL program displays the two-character status code but provides no human-readable error description. The migrated system should map common VSAM status codes (e.g., '35' = file not found, '39' = attributes mismatch, '47' = file not open for read) to descriptive error messages.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions for the retiring COBOL developers: (1) Are these reporting programs run on a regular schedule (daily, weekly, monthly) or only on-demand for audit purposes? (2) Is the SYSOUT output consumed by any downstream process (e.g., a reconciliation script or report formatter), or is it purely for human review? (3) Should the CVV field be masked in the migrated reporting output, and is there a PCI-DSS waiver for the current mainframe batch output? (4) Are there any JCL-level record count checks (IDCAMS REPRO COUNT) that validate the completeness of the dump? (5) Does the CARD-EXPIRAION-DATE misspelling in CVACT02Y match the same misspelling convention used in CVACT01Y (account record)?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
