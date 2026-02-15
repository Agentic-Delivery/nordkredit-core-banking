---
id: "rept-br-007"
title: "Transaction category reference data management"
domain: "reporting"
cobol_source: "TRANCATG.jcl:1-62"
requirement_id: "REPT-BR-007"
regulations:
  - "FFFS 2014:5 Ch. 4 §3 (reference data integrity)"
  - "AML/KYC (transaction classification)"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# REPT-BR-007: Transaction category reference data management

## Summary

The TRANCATG batch job manages the lifecycle of the transaction category type VSAM reference file. This is a destructive reload operation that replaces the entire VSAM KSDS cluster with fresh data from a flat file source. The job executes in three steps: first it deletes the existing VSAM KSDS cluster (tolerating not-found conditions by setting MAXCC=0), then it defines a new VSAM KSDS cluster with a 6-byte key at offset 0 and fixed 60-byte records with INDEXED organization, and finally it loads data from a sequential flat file (TRANCATG.PS) into the newly defined VSAM cluster using IDCAMS REPRO. The transaction category reference data is foundational to the reporting domain, as it defines the valid categories used for classifying and aggregating transactions in reports such as REPT-BR-006.

## Business Logic

### Pseudocode

```
FUNCTION RefreshTransactionCategoryReferenceData():
    // Step 1: Delete existing VSAM cluster (tolerate not-found)
    TRY
        DELETE CLUSTER "TRANCATG.VSAM.KSDS"
    CATCH NotFoundError
        SET MAXCC = 0   // Continue processing, not an error
    END TRY

    // Step 2: Define new VSAM KSDS cluster
    DEFINE CLUSTER "TRANCATG.VSAM.KSDS":
        Type          = INDEXED (KSDS)
        Key           = 6 bytes at offset 0
        Record Size   = 60 bytes (average and maximum)
        Erase         = YES (secure delete on removal)
        Share Options = (2, 3)  // Cross-region: read sharing; Cross-system: all sharing
        Storage Class = Default
    END DEFINE

    // Step 3: Load data from flat file into VSAM
    OPEN INPUT  "TRANCATG.PS" (sequential flat file)
    OPEN OUTPUT "TRANCATG.VSAM.KSDS" (VSAM KSDS)

    REPRO all records from flat file to VSAM:
        FOR EACH record in flat file:
            VALIDATE key uniqueness (6-byte key at offset 0)
            WRITE record to VSAM KSDS
        END FOR

    CLOSE all files
    RETURN success
END FUNCTION
```

### Decision Table

| Step | Operation | Input | Output | Failure Handling |
|------|-----------|-------|--------|------------------|
| STEP05 (DELETE) | Delete existing VSAM cluster | TRANCATG.VSAM.KSDS | Cluster removed | MAXCC=0 if cluster not found |
| STEP10 (DEFINE) | Define new VSAM KSDS cluster | Cluster definition parameters | Empty TRANCATG.VSAM.KSDS created | Job fails if define fails |
| STEP15 (REPRO) | Load flat file into VSAM | TRANCATG.PS (sequential) | TRANCATG.VSAM.KSDS (populated) | Job fails on duplicate keys or I/O errors |

## Source COBOL Reference

**Program:** `TRANCATG.jcl`
**Lines:** 1-62 (Complete JCL job for transaction category reference data management)

STEP05 - delete existing VSAM cluster (lines 22-28):
```jcl
000022 //STEP05   EXEC PGM=IDCAMS
000023 //SYSPRINT DD SYSOUT=*
000024 //SYSIN    DD *
000025   DELETE AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS -
000026          CLUSTER
000027   SET MAXCC = 0
000028 /*
```

STEP10 - define new VSAM KSDS cluster (lines 33-49):
```jcl
000033 //STEP10   EXEC PGM=IDCAMS
000034 //SYSPRINT DD SYSOUT=*
000035 //SYSIN    DD *
000036   DEFINE CLUSTER -
000037          (NAME(AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS) -
000038           INDEXED -
000039           KEYS(6 0) -
000040           RECORDSIZE(60 60) -
000041           ERASE -
000042           SHAREOPTIONS(2 3) -
000043          ) -
000044          DATA -
000045          (NAME(AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS.DATA) -
000046          ) -
000047          INDEX -
000048          (NAME(AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS.INDEX) -
000049          )
```

STEP15 - load data from flat file (lines 54-62):
```jcl
000054 //STEP15   EXEC PGM=IDCAMS
000055 //SYSPRINT DD SYSOUT=*
000056 //INDD     DD DSN=AWS.M2.CARDDEMO.TRANCATG.PS,
000057 //            DISP=SHR
000058 //OUTDD    DD DSN=AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS,
000059 //            DISP=SHR
000060 //SYSIN    DD *
000061   REPRO INFILE(INDD) -
000062         OUTFILE(OUTDD)
```

## Acceptance Criteria

### Scenario 1: Existing VSAM cluster is deleted before recreation

```gherkin
Given a TRANCATG VSAM KSDS cluster exists with previous reference data
When the transaction category reference data management job is executed
Then the existing VSAM cluster is deleted
And the job continues to the define step
```

### Scenario 2: Missing VSAM cluster does not cause job failure

```gherkin
Given no TRANCATG VSAM KSDS cluster exists
When the delete step executes
Then the IDCAMS DELETE returns a not-found condition
And MAXCC is set to 0
And the job continues to the define step without error
```

### Scenario 3: New VSAM cluster is defined with correct attributes

```gherkin
Given the previous cluster has been deleted or did not exist
When the define step creates a new VSAM KSDS cluster
Then the cluster is created with INDEXED organization
And the primary key is 6 bytes at offset 0
And the record size is fixed at 60 bytes
And the ERASE option is enabled for secure deletion
And share options are set to (2, 3)
```

### Scenario 4: Reference data is loaded from flat file

```gherkin
Given an empty TRANCATG VSAM KSDS cluster has been defined
And the flat file TRANCATG.PS contains valid transaction category records
When the REPRO step executes
Then all records from the flat file are loaded into the VSAM cluster
And each record is accessible by its 6-byte key
```

### Scenario 5: Duplicate keys in source file cause load failure

```gherkin
Given the flat file TRANCATG.PS contains two records with the same 6-byte key
When the REPRO step attempts to load the data
Then the REPRO operation fails with a duplicate key error
And the job terminates with a non-zero condition code
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §3 | Financial institutions must ensure the integrity and accuracy of reference data used in financial processing and reporting | The destructive reload pattern ensures the VSAM reference file always reflects the authoritative flat file source, preventing stale or corrupted reference data from affecting transaction categorization |
| AML/KYC | Transaction classification | Institutions must maintain accurate transaction classification systems to support suspicious activity monitoring | Transaction category reference data defines the valid categories for classifying transactions; accurate and complete reference data is essential for reliable AML transaction monitoring and pattern detection |

## Edge Cases

1. **Empty flat file**: If TRANCATG.PS contains no records, the REPRO step completes successfully but leaves the VSAM cluster empty, resulting in no valid transaction categories being available for downstream processing.
2. **Concurrent access during reload**: SHAREOPTIONS(2 3) allows read sharing across regions during the reload, but writing is exclusive. Any concurrent read access during the DELETE/DEFINE/REPRO window will fail until the VSAM cluster is redefined and loaded.
3. **Flat file record length mismatch**: If the flat file contains records that do not match the expected 60-byte VSAM record size, the REPRO operation may fail or produce incorrect key alignment in the loaded data.
4. **Key sequence in flat file**: REPRO into a KSDS does not require the input to be in key sequence; VSAM will insert records by key. However, loading pre-sorted data is more efficient and avoids CI/CA splits.
5. **ERASE option implications**: The ERASE option causes VSAM to overwrite the data component with binary zeros when the cluster is deleted, which is important for data security but adds processing time during deletion.
6. **Partial load failure**: If the REPRO step fails mid-load (e.g., due to disk space), the VSAM cluster will contain a partial dataset. There is no rollback mechanism; the job must be rerun from the DELETE step.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The 6-byte key at offset 0 likely represents a composite key of transaction type code and category code. Confirm the key structure and whether the flat file TRANCATG.PS is manually maintained or generated by another process. The .NET implementation should use a database table with appropriate unique constraints mirroring the VSAM KSDS key definition.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
