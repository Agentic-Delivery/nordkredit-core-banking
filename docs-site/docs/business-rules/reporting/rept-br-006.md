---
id: "rept-br-006"
title: "Transaction category balance report"
domain: "reporting"
cobol_source: "PRTCATBL.jcl:1-63"
requirement_id: "REPT-BR-006"
regulations:
  - "FFFS 2014:5 Ch. 8 §4 (internal controls)"
  - "AML/KYC (transaction category monitoring)"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# REPT-BR-006: Transaction category balance report

## Summary

The PRTCATBL batch job produces a formatted report of transaction category balances from the TCATBALF VSAM KSDS file. The job executes in three steps: first it deletes any existing report output file using IEFBR14, then it unloads the 50-byte VSAM KSDS records into a sequential GDG backup file using REPROC, and finally it sorts the records by account ID, transaction type code, and transaction category code in ascending order using the SORT utility. The sort step applies OUTREC formatting to produce a human-readable report with the balance field edited into a decimal display format (TTTTTTTTT.TT). The final output is a fixed-block report file with 40-byte logical records. This report supports internal control requirements for monitoring transaction category balances and AML/KYC transaction classification oversight.

## Business Logic

### Pseudocode

```
FUNCTION GenerateTransactionCategoryBalanceReport():
    // Step 1: Delete existing report file
    DELETE file "TCATBALF.REPT" IF EXISTS

    // Step 2: Unload VSAM to sequential backup
    OPEN INPUT "TCATBALF.VSAM.KSDS" (50-byte records)
    OPEN OUTPUT "TCATBALF.BKUP" (GDG +1)
    REPRO all records from VSAM to sequential backup
    CLOSE all files

    // Step 3: Sort and format report
    OPEN INPUT "TCATBALF.BKUP"

    DEFINE field mappings:
        TRANCAT-ACCT-ID  = position 1,  length 11, Zoned Decimal
        TRANCAT-TYPE-CD  = position 12, length 2,  Character
        TRANCAT-CD       = position 14, length 4,  Zoned Decimal
        TRAN-CAT-BAL     = position 18, length 11, Zoned Decimal

    SORT records BY:
        TRANCAT-ACCT-ID  ASCENDING
        TRANCAT-TYPE-CD  ASCENDING
        TRANCAT-CD       ASCENDING

    FOR EACH sorted record:
        FORMAT output record (40 bytes):
            TRANCAT-ACCT-ID (as-is)
            TRANCAT-TYPE-CD (as-is)
            TRANCAT-CD (as-is)
            TRAN-CAT-BAL edited as TTTTTTTTT.TT (decimal point inserted)
        WRITE to "TCATBALF.REPT" (LRECL=40, RECFM=FB)
    END FOR

    RETURN report file
END FUNCTION
```

### Decision Table

| Step | Input | Operation | Output | Record Format |
|------|-------|-----------|--------|---------------|
| DELDEF (IEFBR14) | N/A | Delete existing report file | TCATBALF.REPT deleted | N/A |
| STEP05R (REPROC) | TCATBALF.VSAM.KSDS (50-byte KSDS) | Unload VSAM to sequential | TCATBALF.BKUP (GDG +1) | 50-byte sequential |
| STEP10R (SORT) | TCATBALF.BKUP (sequential) | Sort by acct/type/cat + format balance | TCATBALF.REPT | LRECL=40, RECFM=FB |

## Source COBOL Reference

**Program:** `PRTCATBL.jcl`
**Lines:** 1-63 (Complete JCL job for transaction category balance report)

DELDEF step - delete existing report (lines 21-25):
```jcl
000021 //DELDEF   EXEC PGM=IEFBR14
000022 //DD1      DD DSN=AWS.M2.CARDDEMO.TCATBALF.REPT,
000023 //            DISP=(MOD,DELETE,DELETE),
000024 //            SPACE=(TRK,0),
000025 //            UNIT=SYSDA
```

STEP05R - unload VSAM to sequential backup (lines 29-39):
```jcl
000029 //STEP05R  EXEC PGM=REPROC
000030 //SYSPRINT DD SYSOUT=*
000031 //INDD     DD DSN=AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS,
000032 //            DISP=SHR
000033 //OUTDD    DD DSN=AWS.M2.CARDDEMO.TCATBALF.BKUP(+1),
000034 //            DISP=(NEW,CATLG,DELETE),
000035 //            SPACE=(CYL,(5,5),RLSE),
000036 //            UNIT=SYSDA,
000037 //            DCB=(RECFM=FB,LRECL=50,BLKSIZE=500)
000038 //SYSIN    DD *
000039 //  REPRO INFILE(INDD) OUTFILE(OUTDD)
```

STEP10R - sort and format report (lines 43-63):
```jcl
000043 //STEP10R  EXEC PGM=SORT
000044 //SYSOUT   DD SYSOUT=*
000045 //SORTIN   DD DSN=AWS.M2.CARDDEMO.TCATBALF.BKUP(0),
000046 //            DISP=SHR
000047 //SORTOUT  DD DSN=AWS.M2.CARDDEMO.TCATBALF.REPT,
000048 //            DISP=(NEW,CATLG,DELETE),
000049 //            SPACE=(CYL,(5,5),RLSE),
000050 //            UNIT=SYSDA,
000051 //            DCB=(RECFM=FB,LRECL=40,BLKSIZE=400)
000052 //SYMNAMES DD *
000053 //  TRANCAT-ACCT-ID,1,11,ZD
000054 //  TRANCAT-TYPE-CD,12,2,CH
000055 //  TRANCAT-CD,14,4,ZD
000056 //  TRAN-CAT-BAL,18,11,ZD
000057 //SYSIN    DD *
000058 //  SORT FIELDS=(TRANCAT-ACCT-ID,A,
000059 //               TRANCAT-TYPE-CD,A,
000060 //               TRANCAT-CD,A)
000061 //  OUTREC FIELDS=(TRANCAT-ACCT-ID,
000062 //                  TRANCAT-TYPE-CD,
000063 //                  TRANCAT-CD,
000064 //                  TRAN-CAT-BAL,EDIT=(TTTTTTTTT.TT))
```

## Acceptance Criteria

### Scenario 1: Existing report file is cleaned up before generation

```gherkin
Given a previous transaction category balance report file exists
When the transaction category balance report job is executed
Then the existing report file is deleted before new report generation begins
```

### Scenario 2: VSAM data is backed up to GDG before processing

```gherkin
Given the TCATBALF VSAM KSDS file contains transaction category balance records
When the report job executes the backup step
Then all 50-byte records are copied to a new GDG generation of the sequential backup file
And the backup file has RECFM=FB and LRECL=50
```

### Scenario 3: Records are sorted by account, type, and category

```gherkin
Given the sequential backup contains unsorted transaction category balance records
When the sort step processes the records
Then the output records are sorted by account ID ascending
And within the same account ID, sorted by transaction type code ascending
And within the same type code, sorted by transaction category code ascending
```

### Scenario 4: Balance field is formatted with decimal display

```gherkin
Given a transaction category balance record with raw balance value 00012345678
When the sort step formats the output record
Then the balance is displayed as "123456789.78" using the EDIT=(TTTTTTTTT.TT) mask
And leading zeros are suppressed in the formatted output
```

### Scenario 5: Report output has correct record format

```gherkin
Given the sort step has processed all records
When the report file is written
Then each output record is exactly 40 bytes long
And the file has fixed-block record format (RECFM=FB)
And the file contains account ID, type code, category code, and formatted balance fields
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Financial institutions must maintain adequate internal controls including reporting on transaction categorization | The report provides a sorted, formatted view of transaction category balances grouped by account, enabling internal audit and control reviews of transaction classification accuracy |
| AML/KYC | Transaction monitoring | Institutions must monitor and categorize transactions to detect suspicious patterns | Transaction category balance reporting supports AML oversight by providing aggregated balance views per category, enabling analysts to identify unusual category distributions or balance anomalies |

## Edge Cases

1. **Empty VSAM file**: If the TCATBALF VSAM KSDS contains no records, the backup step produces an empty sequential file, and the sort step produces an empty report file with no data rows.
2. **Negative balances**: The EDIT=(TTTTTTTTT.TT) mask for zoned decimal does not explicitly handle sign indicators. If TRAN-CAT-BAL contains negative values (overpunch or separate sign), the formatted output may display incorrectly or produce unexpected characters.
3. **Maximum balance value**: The balance field is 11 bytes of zoned decimal, supporting values up to 99999999999 (before decimal formatting). The EDIT mask displays 9 digits plus 2 decimal places; values exceeding 999999999.99 will overflow the formatted field.
4. **GDG catalog full**: If the GDG base for TCATBALF.BKUP has reached its generation limit, the backup step will fail and must be handled by JCL condition code processing.
5. **Non-existent report file on delete**: The DELDEF step uses DISP=(MOD,DELETE,DELETE) with IEFBR14; if the report file does not exist, the step should still complete successfully (catalog not-found is tolerated).
6. **Sort utility memory constraints**: For very large VSAM files, the SORT step may require additional work space allocation; the current SPACE=(CYL,(5,5),RLSE) may need adjustment for production volumes.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The EDIT=(TTTTTTTTT.TT) mask implies the last two digits of the 11-byte zoned decimal balance represent cents (implied decimal point at position 9.2). Confirm whether TRAN-CAT-BAL is stored with an implied V99 decimal or as whole units. The .NET implementation must preserve the exact same decimal formatting.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
