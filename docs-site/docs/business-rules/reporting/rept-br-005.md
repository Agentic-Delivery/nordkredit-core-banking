---
id: "rept-br-005"
title: "Report file generation data group management"
domain: "reporting"
cobol_source: "REPTFILE.jcl:1-30"
requirement_id: "REPT-BR-005"
regulations:
  - "FFFS 2014:5 Ch. 8 §4 (records retention)"
  - "DORA Art. 12 (backup and recovery)"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# REPT-BR-005: Report file generation data group management

## Summary

The REPTFILE.jcl job defines a Generation Data Group (GDG) base for the transaction report output dataset using the IDCAMS utility. The GDG base named AWS.M2.CARDDEMO.TRANREPT is configured with a LIMIT of 10, meaning the system retains up to 10 generations (versions) of the transaction report output. Each execution of the TRANREPT.jcl batch job creates a new generation with a (+1) suffix, and when the 11th generation is created, the oldest generation is automatically scratched (deleted). This GDG mechanism provides a rolling retention window of report outputs, enabling access to historical report versions for audit, comparison, and recovery purposes.

## Business Logic

### Pseudocode

```
PROCEDURE DefineReportGDG():

    -- Define GDG base catalog entry
    DEFINE GDG:
        NAME = "AWS.M2.CARDDEMO.TRANREPT"
        LIMIT = 10
        SCRATCH           -- delete oldest when limit exceeded
        NOEMPTY            -- do not delete all when limit exceeded

    -- GDG usage pattern (by TRANREPT.jcl):
    -- Each report run creates TRANREPT(+1)  -- new generation
    -- Current report is TRANREPT(0)         -- most recent
    -- Previous report is TRANREPT(-1)       -- one version back
    -- Oldest retained is TRANREPT(-9)       -- 10th oldest

END PROCEDURE

PROCEDURE AccessReportGeneration(relativeNumber):
    -- relativeNumber: +1 (new), 0 (current), -1 to -9 (historical)
    IF relativeNumber = +1 THEN
        CREATE new generation
        IF generationCount > LIMIT THEN
            DELETE oldest generation
        END IF
    ELSE
        OPEN existing generation for read
    END IF
END PROCEDURE
```

### Decision Table

| Generation Reference | Meaning | Access Mode | Action When Limit Exceeded |
|---------------------|---------|-------------|---------------------------|
| TRANREPT(+1) | New generation | Write (create) | Oldest generation automatically deleted |
| TRANREPT(0) | Most recent generation | Read | N/A |
| TRANREPT(-1) | Previous generation | Read | N/A |
| TRANREPT(-N) where N=1..9 | Nth previous generation | Read | N/A |
| TRANREPT(-10) or older | Beyond retention limit | N/A | Already deleted |

## Source COBOL Reference

**Program:** `REPTFILE.jcl`
**Lines:** 1-30 (GDG base definition)

IDCAMS GDG definition:
```jcl
//REPTFILE JOB ACCT#,REPTFILE,MSGCLASS=X,
//             CLASS=A,NOTIFY=&SYSUID
//*
//* DEFINE GDG BASE FOR TRANSACTION REPORT OUTPUT
//*
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
  DEFINE GDG -
         (NAME(AWS.M2.CARDDEMO.TRANREPT) -
          LIMIT(10) -
          SCRATCH -
          NOEMPTY)
/*
```

GDG usage in TRANREPT.jcl (report output):
```jcl
//TRANREPT DD  DSN=AWS.M2.CARDDEMO.TRANREPT(+1),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(5,2),RLSE),
//             DCB=(RECFM=FB,LRECL=133,BLKSIZE=0)
```

## Acceptance Criteria

### Scenario 1: New report generation created

```gherkin
Given the TRANREPT GDG base is defined with a limit of 10
When the TRANREPT.jcl batch job runs successfully
Then a new generation of the TRANREPT dataset is created
And the new generation is cataloged and accessible as TRANREPT(0)
And the previous generation becomes TRANREPT(-1)
```

### Scenario 2: Retention limit enforced

```gherkin
Given the TRANREPT GDG already contains 10 generations
When a new report generation is created via TRANREPT(+1)
Then the oldest generation (previously TRANREPT(-9)) is deleted
And exactly 10 generations remain in the GDG
```

### Scenario 3: Historical report access

```gherkin
Given the TRANREPT GDG contains multiple generations
When an auditor requests access to the 3rd most recent report
Then the system retrieves TRANREPT(-2)
And the report content matches the output from the 3rd most recent batch run
```

### Scenario 4: GDG base definition is idempotent

```gherkin
Given the TRANREPT GDG base already exists
When the REPTFILE.jcl job is run again
Then the IDCAMS utility reports that the GDG base already exists
And existing generations are not affected
```

### Scenario 5: Report retention meets regulatory minimum

```gherkin
Given the reporting schedule produces one report per business day
And the GDG limit is 10 generations
Then the system retains approximately 2 weeks of daily reports
And reports older than the retention window must be archived separately if regulatory retention requires longer periods
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Financial institutions must maintain adequate records of transactions and operations for a defined retention period | GDG with LIMIT(10) provides automated retention of the 10 most recent report generations, ensuring a rolling window of historical reports is always available for audit and review |
| DORA | Art. 12 | ICT systems must include backup policies and procedures, and restoration and recovery procedures | GDG management provides versioned report backups with automatic lifecycle management, supporting recovery of recent report outputs without manual intervention |

## Edge Cases

1. **GDG base not yet defined**: If TRANREPT.jcl runs before REPTFILE.jcl has defined the GDG base, the job will fail with a catalog error. The .NET deployment must ensure equivalent storage structures are provisioned before batch jobs execute.
2. **LIMIT(10) insufficient for regulatory retention**: Ten generations may not satisfy all regulatory retention requirements (e.g., FSA may require 5-7 years of records). The .NET implementation should implement configurable retention policies that can exceed the original 10-generation limit.
3. **Concurrent GDG creation**: If two instances of TRANREPT.jcl run simultaneously, both may attempt to create TRANREPT(+1), causing a catalog conflict. The .NET implementation must handle concurrent report generation with unique identifiers or locking.
4. **SCRATCH vs NOSCRATCH**: The SCRATCH option means the oldest generation's data is physically deleted when the limit is exceeded. If the option were NOSCRATCH, only the catalog entry would be removed. The .NET equivalent must ensure physical deletion of expired report files (Azure Blob lifecycle management).
5. **NOEMPTY behavior**: The NOEMPTY option means that when the GDG limit is exceeded, only the oldest generation is removed (not all generations). This is the standard behavior. If EMPTY were specified, all generations would be deleted when the limit is exceeded.
6. **Disk space for 10 generations**: Each report generation uses SPACE=(CYL,(5,2)), meaning up to 7 cylinders per generation. With 10 generations retained, the total space could be significant. The .NET implementation should monitor Azure Blob storage costs for retained reports.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Confirm whether the 10-generation limit is sufficient for all regulatory and business retention requirements. If reports are generated daily, 10 generations cover approximately 2 weeks. Determine whether a separate archival process exists for long-term retention, and whether that process needs to be migrated as well.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
