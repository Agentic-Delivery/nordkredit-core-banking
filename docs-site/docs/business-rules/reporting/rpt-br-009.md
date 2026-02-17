---
id: "rpt-br-009"
title: "Regulatory data retention and archival rules"
domain: "reporting"
cobol_source: "CBTRN03C.cbl:1-650 (report output lifecycle), CBTRN02C.cbl:1-723 (transaction data lifecycle)"
requirement_id: "RPT-BR-009"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 7"
  - "AML 2017:11 Para. 3"
  - "GDPR Art. 5(1)(e)"
  - "GDPR Art. 17"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-009: Regulatory data retention and archival rules

## Summary

NordKredit AB must retain financial reports, transaction data, and regulatory filings for defined periods to satisfy overlapping regulatory requirements. FSA FFFS 2014:5 Ch. 3 requires accounting records to be retained for a minimum of 7 years. AML 2017:11 requires transaction monitoring records (including AML screening reports) to be retained for at least 5 years after the end of the business relationship. GDPR Art. 5(1)(e) limits storage to the period necessary for the purpose, and Art. 17 grants data subjects the right to erasure — creating a direct tension with financial record retention obligations. The mainframe system retains report output files (TRANREPT from CBTRN03C) and transaction files (TRANSACT from CBTRN02C) in VSAM datasets and GDG (Generation Data Group) archives with operator-managed retention schedules. The migrated system must implement automated, regulation-aware retention policies in Azure Blob Storage with lifecycle management. Extracted from the batch pipeline data lifecycle and regulatory retention requirements.

## Business Logic

### Pseudocode

```
DATA RETENTION FRAMEWORK:

    RETENTION PERIODS BY DATA TYPE:

        FINANCIAL REPORTS (daily, monthly, quarterly, annual):
            Retention: 7 years from report generation date
            Regulation: FSA FFFS 2014:5 Ch. 3
            Storage: Immutable archive after generation
            Format: Original format + migrated format (for comparison)

        TRANSACTION DATA (TRANSACT file, posted transactions):
            Retention: 7 years from transaction processing date
            Regulation: FSA FFFS 2014:5 Ch. 3
            Note: Also supports AML 5-year requirement (subset)

        AML SCREENING REPORTS (AMLREPT, SARALERT):
            Retention: 5 years after end of business relationship
            Regulation: AML 2017:11 Para. 3
            Note: Retention clock starts at relationship end, not report date
            Special: SAR filings retained indefinitely per Finanspolisen guidance

        CUSTOMER STATEMENTS (STMTREPT):
            Retention: 7 years from statement generation date
            Regulation: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 57
            GDPR: Customer can request copy (Art. 15) but not erasure
                   (legal obligation exemption under Art. 17(3)(b))

        DORA INCIDENT RECORDS:
            Retention: 5 years from incident resolution
            Regulation: DORA Art. 17
            Note: Major incident reports retained with full audit trail

        MANAGEMENT REPORTS:
            Retention: 3 years from generation (internal policy)
            Note: Not subject to external regulatory minimum

    RETENTION LIFECYCLE:

        FOR EACH report/data file generated:
            1. ACTIVE phase (0-90 days):
               Store in hot storage (Azure Blob, hot tier)
               Full read/write access for authorized users
               Used for daily operations and queries

            2. WARM phase (91 days - 2 years):
               Move to cool storage (Azure Blob, cool tier)
               Read access for authorized users
               Used for historical queries and audits

            3. ARCHIVE phase (2 years - retention end):
               Move to archive storage (Azure Blob, archive tier)
               Read access with rehydration delay (hours)
               Used for regulatory audits and legal discovery

            4. DELETION phase (after retention period):
               Verify no legal hold or pending audit
               Delete with audit trail of deletion
               Log deletion event for compliance

    GDPR ERASURE REQUEST HANDLING:

        WHEN customer requests data erasure (Art. 17):
            1. Check legal obligation exemptions (Art. 17(3)(b)):
               IF data required for FSA record retention (7 years):
                   DENY erasure for financial records
                   Inform customer of legal obligation basis
               IF data required for AML retention (5 years):
                   DENY erasure for AML records
                   Inform customer of legal obligation basis
            2. FOR data not subject to legal retention:
               Execute erasure within 30 days
               Log erasure for compliance audit
            3. FOR data past retention period:
               Execute erasure as part of normal lifecycle

    MAINFRAME ARCHIVE PATTERN:

        Current mainframe retention:
            GDG (Generation Data Group) for sequential datasets:
                TRANSACT file: GDG with LIMIT(365) — 365 generations (1 year daily)
                TRANREPT file: GDG with LIMIT(365) — 365 daily report generations
                Beyond GDG limit: archived to tape by operator schedule
            VSAM retention:
                Managed by SMS (Storage Management Subsystem)
                Retention class assigned at dataset creation
            Tape archive:
                Operator-scheduled archive cycles
                Manual catalog management
                7-year retention for regulatory compliance
```

### Retention Matrix

| Data Type | Regulatory Minimum | NordKredit Policy | Regulation | GDPR Erasure |
|---|---|---|---|---|
| Daily transaction reports | 7 years | 7 years | FSA Ch. 3 | Exempt (legal obligation) |
| Customer statements | 7 years | 7 years | FSA Ch. 3, PSD2 | Exempt (legal obligation) |
| AML screening reports | 5 years post-relationship | 7 years | AML 2017:11 | Exempt (legal obligation) |
| SAR filings | Indefinite | Indefinite | Finanspolisen | Exempt (legal obligation) |
| FSA regulatory reports | 7 years | 10 years | FSA Ch. 3/7 | Exempt (legal obligation) |
| DORA incident records | 5 years | 7 years | DORA Art. 17 | Exempt (legal obligation) |
| Management reports | None (internal) | 3 years | N/A | Subject to erasure |
| Transaction source data | 7 years | 7 years | FSA Ch. 3 | Exempt (legal obligation) |

### Decision Table — Erasure Request Handling

| Data Type | Retention Period Active | Legal Hold | Action |
|---|---|---|---|
| Financial report | Yes | No | Deny erasure, cite FSA Ch. 3 |
| Financial report | Yes | Yes | Deny erasure, cite legal hold + FSA Ch. 3 |
| Financial report | No | No | Delete with audit log |
| Financial report | No | Yes | Deny erasure, cite legal hold |
| AML report | Yes (5yr post-relationship) | No | Deny erasure, cite AML 2017:11 |
| Management report | Yes (3yr internal) | No | Deny erasure, cite legitimate interest |
| Management report | No | No | Delete with audit log |

## Source COBOL Reference

**Programs:** `CBTRN03C.cbl` (650 lines — report file output), `CBTRN02C.cbl` (723 lines — transaction file output)

The mainframe report lifecycle is managed through JCL DD statements and SMS policies:

```jcl
//TRANREPT DD DSN=NORDKRED.DALYREPT.G(+1),
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(RECFM=FBA,LRECL=133,BLKSIZE=13300),
//            SPACE=(CYL,(5,2)),
//            RETPD=2555
```

The `RETPD=2555` parameter specifies a retention period of 2,555 days (approximately 7 years). The `G(+1)` suffix indicates GDG (Generation Data Group) — each daily run creates a new generation, and older generations are archived to tape when the GDG limit is reached.

CBTRN03C writes the report output file:
```cobol
000084       FD  REPORT-FILE.
000085       01 FD-REPTFILE-REC       PIC X(133).
```

The 133-character report records are written sequentially to TRANREPT. Once the program completes (GOBACK at line 217), the file is closed and the JCL output disposition (`DISP=(NEW,CATLG,DELETE)`) catalogs the dataset for retention management.

**Note:** Dedicated archival COBOL programs are not available in the repository. The retention lifecycle is managed by JCL parameters, SMS policies, and operator-scheduled tape archival procedures. The migrated system must replace these with Azure Blob Storage lifecycle management policies.

## Acceptance Criteria

### Scenario 1: 7-year retention for financial reports

```gherkin
GIVEN a daily transaction report generated on 2026-01-15
WHEN 7 years have elapsed (2033-01-15)
  AND no legal hold is active
THEN the report remains accessible for the full 7-year period
  AND after 7 years, the report is eligible for automated deletion
  AND a deletion audit record is created
```

### Scenario 2: AML report retention tied to relationship

```gherkin
GIVEN an AML screening report flagged account "12345678901" on 2026-03-15
  AND the customer's business relationship ended on 2028-06-30
WHEN 5 years have elapsed since relationship end (2033-06-30)
THEN the AML report is retained until at least 2033-06-30
  AND SAR filing records (if any) are retained indefinitely
```

### Scenario 3: GDPR erasure request denied for retained data

```gherkin
GIVEN a customer requests erasure of their account data under GDPR Art. 17
  AND the customer's transaction reports are within the 7-year FSA retention period
WHEN the erasure request is evaluated
THEN the request is denied for financial records
  AND the customer is informed of the legal obligation basis (Art. 17(3)(b))
  AND data not subject to legal retention (e.g., marketing preferences) is erased
```

### Scenario 4: Storage tier lifecycle transition

```gherkin
GIVEN a daily report is generated and stored in hot-tier Azure Blob Storage
WHEN 91 days have elapsed since generation
THEN the report is automatically moved to cool-tier storage
  AND when 2 years have elapsed, it is moved to archive-tier storage
  AND the report remains accessible (with rehydration delay) throughout
```

### Scenario 5: Legal hold prevents deletion

```gherkin
GIVEN a management report has exceeded its 3-year retention period
  AND a legal hold has been placed due to an ongoing regulatory investigation
WHEN the automated deletion process evaluates the report
THEN the deletion is blocked by the legal hold
  AND the report is retained until the legal hold is lifted
  AND the legal hold status is logged for audit
```

### Scenario 6: Mainframe archive migration

```gherkin
GIVEN historical reports exist on mainframe tape archives
WHEN the migration to Azure is executed
THEN all reports within their retention period are migrated to Azure Blob Storage
  AND reports are placed in the appropriate storage tier based on age
  AND original mainframe format is preserved alongside migrated format
  AND a migration audit trail records each file transferred
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records must be retained for a minimum of 7 years | 7-year retention policy for all financial reports and transaction data |
| FSA FFFS 2014:5 | Ch. 7 | Reporting systems must maintain adequate records | Structured retention lifecycle with tiered storage and audit trails |
| AML 2017:11 | Para. 3 | Transaction monitoring records retained 5 years post-relationship | AML-specific retention clock tied to business relationship end date |
| GDPR | Art. 5(1)(e) | Storage limitation — data kept only as long as necessary | Automated lifecycle management with defined retention periods per data type |
| GDPR | Art. 17 | Right to erasure — data subjects can request deletion | Erasure evaluation framework with legal obligation exemptions (Art. 17(3)(b)) |

## Edge Cases

1. **Overlapping retention periods**: A customer statement contains both financial data (7-year FSA retention) and PII (GDPR storage limitation). The longest applicable retention period governs — the full document is retained for 7 years, with GDPR exemption under Art. 17(3)(b).

2. **Business relationship end date ambiguity**: AML retention is measured from the end of the business relationship. For dormant accounts that are not formally closed, the relationship end date is undefined. The system must define a dormancy-to-closure threshold (e.g., 3 years of inactivity) or retain AML records indefinitely for open accounts.

3. **Cross-border data transfer during archival**: If archive storage is replicated across Azure regions for disaster recovery, all replicas must remain within EU boundaries per GDPR. Azure geo-redundant storage must be configured for EU-only regions (e.g., North Europe, West Europe).

4. **Archive rehydration SLA**: Archive-tier Azure Blob Storage requires rehydration before access (up to 15 hours). If a regulatory auditor requests immediate access to archived reports, the system must have a process for expedited rehydration or maintain a catalog of archived reports for quick identification.

5. **Format obsolescence**: Reports retained for 7+ years may outlive the software that generated them. The system must store reports in open, self-describing formats (e.g., PDF/A for visual reports, CSV/JSON with schema for data reports) to ensure long-term accessibility.

6. **Retention period extension**: Regulatory changes may extend minimum retention periods. The system must support retroactive extension of retention for data that would otherwise be eligible for deletion.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What is the current mainframe tape archival schedule and retention policy? (2) Are there any existing legal holds on historical data? (3) What is the defined dormancy period for account closure determination? (4) Does NordKredit have an existing data retention policy document, or is this the first formal definition? (5) How are GDPR erasure requests currently handled for financial records? (6) What Azure regions are approved for data residency?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
