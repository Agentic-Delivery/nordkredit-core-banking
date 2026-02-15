---
id: "rept-br-004"
title: "Transaction report date filtering and sorting"
domain: "reporting"
cobol_source: "TRANREPT.jcl:1-80"
requirement_id: "REPT-BR-004"
regulations:
  - "FSA FFFS 2014:5 (regulatory reporting)"
  - "AML/KYC (transaction monitoring reports)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# REPT-BR-004: Transaction report date filtering and sorting

## Summary

The batch transaction report pipeline, defined in TRANREPT.jcl, processes transactions through three sequential steps. First, STEP05R (REPROC) unloads the TRANSACT VSAM KSDS dataset into a sequential backup in the TRANSACT.BKUP generation data group. Second, STEP05R (SORT) filters transactions by processing date within the parameterized start and end date range (supplied by the online program CORPT00C) and sorts the results by card number in ascending order. The SORT step uses SYMNAMES to define field positions: TRAN-CARD-NUM at byte offset 263 (16 bytes, zoned decimal) and TRAN-PROC-DT at byte offset 305 (10 bytes, character). The filtered and sorted output is written to the TRANSACT.DALY generation data group. Third, STEP10R executes program CBTRN03C, which reads the filtered transactions along with card cross-reference, transaction type, transaction category, and date parameter files to produce a formatted 133-character fixed-block report output in the TRANREPT generation data group.

## Business Logic

### Pseudocode

```
PROCEDURE GenerateTransactionReport(startDate, endDate):

    -- Step 1: Backup VSAM to sequential file
    UNLOAD TRANSACT.VSAM.KSDS TO TRANSACT.BKUP(+1)

    -- Step 2: Filter and sort transactions
    DEFINE SYMNAMES:
        TRAN-CARD-NUM = offset 263, length 16, type ZD (zoned decimal)
        TRAN-PROC-DT  = offset 305, length 10, type CH (character)

    SORT inputFile BY TRAN-CARD-NUM ASCENDING

    INCLUDE WHERE:
        TRAN-PROC-DT >= startDate
        AND TRAN-PROC-DT <= endDate

    WRITE sorted/filtered records TO TRANSACT.DALY(+1)

    -- Step 3: Generate formatted report
    READ TRANSACT.DALY          -- filtered transactions
    READ CARDXREF               -- card cross-reference data
    READ TRANTYPE               -- transaction type descriptions
    READ TRANCATG               -- transaction category descriptions
    READ DATEPARM               -- date parameters for report header

    EXECUTE CBTRN03C:
        FORMAT report with LRECL=133, RECFM=FB
        WRITE TO TRANREPT(+1)

END PROCEDURE
```

### Decision Table

| Step | Input | Processing | Output | Failure Condition |
|------|-------|-----------|--------|-------------------|
| STEP05R (REPROC) | TRANSACT.VSAM.KSDS | VSAM unload to sequential | TRANSACT.BKUP(+1) | VSAM read error, disk full |
| STEP05R (SORT) | Sequential backup + SYMNAMES + DATEPARM | Filter by date range, sort by card number | TRANSACT.DALY(+1) | No records match date range, sort failure |
| STEP10R (CBTRN03C) | TRANSACT.DALY + CARDXREF + TRANTYPE + TRANCATG + DATEPARM | Format 133-char report lines | TRANREPT(+1) | Missing reference data, format error |

## Source COBOL Reference

**Program:** `TRANREPT.jcl`
**Lines:** 1-80 (Complete batch job definition)

STEP05R - VSAM unload to sequential backup (lines 23-33):
```jcl
//STEP05R  EXEC PGM=REPROC
//SYSOUT   DD  SYSOUT=*
//VSAMIN   DD  DSN=AWS.M2.CARDDEMO.TRANSACT.VSAM.KSDS,
//             DISP=SHR
//SEQOUT   DD  DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(+1),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(5,2),RLSE),
//             DCB=(RECFM=FB,LRECL=350,BLKSIZE=0)
```

STEP05R - SORT with filtering (lines 37-55):
```jcl
//STEP10R  EXEC PGM=SORT
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=AWS.M2.CARDDEMO.TRANSACT.BKUP(0),
//             DISP=SHR
//SORTOUT  DD  DSN=AWS.M2.CARDDEMO.TRANSACT.DALY(+1),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(5,2),RLSE),
//             DCB=(RECFM=FB,LRECL=350,BLKSIZE=0)
//SYMNAMES DD  *
//  TRAN-CARD-NUM,263,16,ZD
//  TRAN-PROC-DT,305,10,CH
//SYSIN    DD  *
//  SORT FIELDS=(TRAN-CARD-NUM,A)
//  INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,
//                AND,
//                TRAN-PROC-DT,LE,PARM-END-DATE)
//DATEPARM DD  DSN=&&DATEPARM,DISP=SHR
```

STEP10R - Report generation (lines 59-80):
```jcl
//STEP20R  EXEC PGM=CBTRN03C
//SYSOUT   DD  SYSOUT=*
//TRANFILE DD  DSN=AWS.M2.CARDDEMO.TRANSACT.DALY(0),
//             DISP=SHR
//CARDXREF DD  DSN=AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS,
//             DISP=SHR
//TRANTYPE DD  DSN=AWS.M2.CARDDEMO.TRANTYPE.VSAM.KSDS,
//             DISP=SHR
//TRANCATG DD  DSN=AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS,
//             DISP=SHR
//DATEPARM DD  DSN=&&DATEPARM,DISP=SHR
//TRANREPT DD  DSN=AWS.M2.CARDDEMO.TRANREPT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(5,2),RLSE),
//             DCB=(RECFM=FB,LRECL=133,BLKSIZE=0)
```

## Acceptance Criteria

### Scenario 1: Transactions filtered by date range

```gherkin
Given the TRANSACT VSAM dataset contains transactions with various processing dates
When the batch report job runs with start date "2025-01-01" and end date "2025-01-31"
Then only transactions with TRAN-PROC-DT between "2025-01-01" and "2025-01-31" inclusive are included
And transactions outside this range are excluded from the output
```

### Scenario 2: Transactions sorted by card number

```gherkin
Given the filtered transaction set contains records for multiple card numbers
When the SORT step processes the records
Then the output is sorted by TRAN-CARD-NUM in ascending order
And records with the same card number maintain their relative order
```

### Scenario 3: Report output format

```gherkin
Given the sorted and filtered transactions have been produced
When CBTRN03C generates the report
Then the output has a fixed record length of 133 characters
And the output uses fixed-block record format
And the report is written to the TRANREPT generation data group
```

### Scenario 4: No transactions in date range

```gherkin
Given the TRANSACT dataset contains no transactions within the specified date range
When the batch report job runs
Then the SORT step produces an empty output file
And CBTRN03C generates a report with header information but no transaction detail lines
```

### Scenario 5: Reference data enrichment

```gherkin
Given filtered transactions reference card numbers, transaction types, and categories
When CBTRN03C processes the transactions
Then each transaction is enriched with card cross-reference data from CARDXREF
And transaction type descriptions are looked up from TRANTYPE
And transaction category descriptions are looked up from TRANCATG
And the date range parameters are included in the report header from DATEPARM
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Regulatory reporting | Financial institutions must produce accurate and timely transaction reports for regulatory oversight | The pipeline ensures transaction reports are generated from authoritative data (VSAM), filtered by precise date ranges, sorted for readability, and enriched with reference data for completeness |
| AML/KYC | Transaction monitoring | Institutions must be able to produce transaction reports for suspicious activity investigation and audit trail | Date-filtered transaction reports sorted by card number enable investigators to review all transactions for a specific card within a time period, supporting AML screening and suspicious transaction reporting |

## Edge Cases

1. **VSAM dataset locked during unload**: If the TRANSACT VSAM KSDS is locked by an online transaction during STEP05R unload, the job may fail or read inconsistent data. The .NET implementation should use snapshot isolation or read-committed isolation on Azure SQL.
2. **SORT field offset changes**: The SYMNAMES define field positions by byte offset (263, 305). If the TRANSACT record layout changes, these offsets must be updated. The .NET implementation should use named fields rather than byte offsets.
3. **Date format mismatch**: TRAN-PROC-DT is a 10-byte character field. The date format must match between the online program (CORPT00C) and the stored transaction data. Verify the format is consistently YYYY-MM-DD.
4. **GDG generation overflow**: Each step creates a new GDG generation (+1). If the GDG limit is reached, the oldest generation is scratched. Ensure the .NET implementation has equivalent retention policies.
5. **Large transaction volume**: The SORT step processes the entire unloaded VSAM file. For high-volume periods, the sort may require significant temporary space. The .NET equivalent should consider pagination or streaming for large datasets.
6. **Missing reference data**: If a transaction references a card number not in CARDXREF or a transaction type not in TRANTYPE, CBTRN03C behavior is undefined. The .NET implementation should handle missing reference data gracefully with default values or warnings.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Confirm the exact format of TRAN-PROC-DT (YYYY-MM-DD vs other formats) and whether the SORT INCLUDE condition uses string comparison or date comparison semantics. Also clarify the CBTRN03C report layout -- obtain a sample report output to define the exact column positions and formatting rules for the .NET implementation.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
