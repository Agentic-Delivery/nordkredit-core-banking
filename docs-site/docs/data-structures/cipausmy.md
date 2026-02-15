---
id: DS-AUTH-SUM-001
title: "IMS Pending Authorization Summary"
copybook_name: "CIPAUSMY.cpy"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "dbo.PendingAuthorizationSummary"
sidebar_position: 40
---

# DS-AUTH-SUM-001: IMS Pending Authorization Summary (CIPAUSMY)

## Overview

The `CIPAUSMY.cpy` copybook defines the **IMS Pending Authorization Summary Record**, an IMS database segment that stores account-level aggregated authorization data. This record maintains credit/cash limits, current balances, and running counts and amounts of approved and declined authorizations per account.

This summary record is critical for **real-time authorization decisions** -- when a new authorization request arrives, the system reads this summary to check available credit/cash limits and balance before approving or declining the transaction. The record uses COMP-3 (packed decimal) encoding for all financial fields and COMP (binary) encoding for counters.

**Source file:** `CIPAUSMY.cpy`
**Used by:** `CCPAUTH1` (IMS authorization program)

## Source COBOL

```cobol
     05  PA-ACCT-ID                   PIC S9(11) COMP-3.
     05  PA-CUST-ID                   PIC  9(09).
     05  PA-AUTH-STATUS               PIC  X(01).
     05  PA-ACCOUNT-STATUS            PIC  X(02) OCCURS 5 TIMES.
     05  PA-CREDIT-LIMIT              PIC S9(09)V99 COMP-3.
     05  PA-CASH-LIMIT                PIC S9(09)V99 COMP-3.
     05  PA-CREDIT-BALANCE            PIC S9(09)V99 COMP-3.
     05  PA-CASH-BALANCE              PIC S9(09)V99 COMP-3.
     05  PA-APPROVED-AUTH-CNT         PIC S9(04) COMP.
     05  PA-DECLINED-AUTH-CNT         PIC S9(04) COMP.
     05  PA-APPROVED-AUTH-AMT         PIC S9(09)V99 COMP-3.
     05  PA-DECLINED-AUTH-AMT         PIC S9(09)V99 COMP-3.
     05  FILLER                       PIC X(34).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Byte Length | Description | Target Column |
|---|------------|-----------|------|------------|-------------|---------------|
| 1 | `PA-ACCT-ID` | `S9(11) COMP-3` | Packed decimal | 6 | Account identifier | `AccountId` (BIGINT, PK) |
| 2 | `PA-CUST-ID` | `9(09)` | Zoned decimal | 9 | Customer identifier | `CustomerId` (BIGINT) |
| 3 | `PA-AUTH-STATUS` | `X(01)` | Alphanumeric | 1 | Account authorization status | `AuthorizationStatus` (NVARCHAR(20)) |
| 4 | `PA-ACCOUNT-STATUS` | `X(02) OCCURS 5` | Alphanumeric array | 10 | Multiple account status codes (5 entries) | `AccountStatus1`-`AccountStatus5` (CHAR(2)) |
| 5 | `PA-CREDIT-LIMIT` | `S9(09)V99 COMP-3` | Packed decimal | 6 | Credit limit for the account | `CreditLimit` (DECIMAL(11,2)) |
| 6 | `PA-CASH-LIMIT` | `S9(09)V99 COMP-3` | Packed decimal | 6 | Cash advance limit | `CashLimit` (DECIMAL(11,2)) |
| 7 | `PA-CREDIT-BALANCE` | `S9(09)V99 COMP-3` | Packed decimal | 6 | Current credit balance (used portion) | `CreditBalance` (DECIMAL(11,2)) |
| 8 | `PA-CASH-BALANCE` | `S9(09)V99 COMP-3` | Packed decimal | 6 | Current cash advance balance | `CashBalance` (DECIMAL(11,2)) |
| 9 | `PA-APPROVED-AUTH-CNT` | `S9(04) COMP` | Binary | 2 | Count of approved authorizations | `ApprovedAuthCount` (INT) |
| 10 | `PA-DECLINED-AUTH-CNT` | `S9(04) COMP` | Binary | 2 | Count of declined authorizations | `DeclinedAuthCount` (INT) |
| 11 | `PA-APPROVED-AUTH-AMT` | `S9(09)V99 COMP-3` | Packed decimal | 6 | Total approved authorization amount | `ApprovedAuthAmount` (DECIMAL(11,2)) |
| 12 | `PA-DECLINED-AUTH-AMT` | `S9(09)V99 COMP-3` | Packed decimal | 6 | Total declined authorization amount | `DeclinedAuthAmount` (DECIMAL(11,2)) |
| 13 | `FILLER` | `X(34)` | Filler | 34 | Reserved/unused space | Not migrated |

## Field Notes

1. **PA-ACCT-ID** (`PIC S9(11) COMP-3`) -- Account identifier stored as packed decimal. This is the segment key in IMS and maps to the primary key in the target table. COMP-3 encoding stores 11 digits plus sign in 6 bytes.

2. **PA-CUST-ID** (`PIC 9(09)`) -- Customer identifier in standard zoned decimal (display) format. Links the account summary to the customer record. Note this is unsigned (`9` not `S9`), indicating only non-negative values.

3. **PA-AUTH-STATUS** (`PIC X(01)`) -- Single-character authorization status for the account. Determines whether the account is eligible to receive new authorizations. Possible values should be verified from the `CCPAUTH1` program logic.

4. **PA-ACCOUNT-STATUS** (`PIC X(02) OCCURS 5 TIMES`) -- An array of 5 two-character status codes. The OCCURS clause creates 5 repetitions of the field, allowing multiple status indicators per account. In COBOL, these are accessed as `PA-ACCOUNT-STATUS(1)` through `PA-ACCOUNT-STATUS(5)`. The meaning of each position should be verified from the program logic. In the target schema, these are normalized to individual columns (`AccountStatus1` through `AccountStatus5`) since the array size is fixed and small.

5. **PA-CREDIT-LIMIT / PA-CASH-LIMIT** (`PIC S9(09)V99 COMP-3`) -- The credit and cash advance limits for the account. These are the maximum amounts available. The authorization decision compares the requested amount plus the current balance against these limits:
   - Credit check: `PA-CREDIT-BALANCE + requested_amount <= PA-CREDIT-LIMIT`
   - Cash check: `PA-CASH-BALANCE + requested_amount <= PA-CASH-LIMIT`

6. **PA-CREDIT-BALANCE / PA-CASH-BALANCE** (`PIC S9(09)V99 COMP-3`) -- Current used balance against the credit and cash limits. Updated in real-time as authorizations are approved. These are the running totals that determine available credit.

7. **PA-APPROVED-AUTH-CNT / PA-DECLINED-AUTH-CNT** (`PIC S9(04) COMP`) -- Binary (COMP) counters for approved and declined authorizations. `COMP` stores data in native binary format (2 bytes for `S9(04)`). These counters support operational monitoring and fraud detection (high decline rates may indicate suspicious activity).

8. **PA-APPROVED-AUTH-AMT / PA-DECLINED-AUTH-AMT** (`PIC S9(09)V99 COMP-3`) -- Running totals of approved and declined authorization amounts. Used for reconciliation and reporting.

9. **FILLER** (`PIC X(34)`) -- Reserved space for future expansion. Not migrated.

## COMP-3 and COMP Encoding Summary

| Encoding | Storage | Digits Per Byte | Sign Location | Example |
|----------|---------|----------------|---------------|---------|
| COMP-3 (Packed) | BCD | 2 digits per byte | Last nibble (C/D/F) | `S9(09)V99` = 6 bytes |
| COMP (Binary) | Native binary | N/A | Two's complement | `S9(04)` = 2 bytes (halfword) |
| Display (Zoned) | ASCII/EBCDIC | 1 digit per byte | Zone nibble of last byte | `9(09)` = 9 bytes |

## Target Architecture Mapping

| Aspect | COBOL/IMS (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Storage** | IMS hierarchical database segment | Azure SQL relational table |
| **Real-time access** | IMS GU call by account key | Entity Framework `FindAsync` by AccountId |
| **Limit checks** | COBOL arithmetic comparison | Domain service method with business rules |
| **OCCURS array** | `PA-ACCOUNT-STATUS(1)` through `(5)` | Individual columns or JSON array column |
| **Counters** | COMP binary integers | `int` type |
| **Financial amounts** | COMP-3 packed decimal | `decimal` type |
| **Concurrency** | IMS GHU (Get Hold Unique) + REPL | Optimistic concurrency with EF Core `RowVersion` |

### .NET Domain Model

```csharp
public class PendingAuthorizationSummary
{
    public long AccountId { get; set; }
    public long CustomerId { get; set; }
    public string AuthorizationStatus { get; set; } = default!;
    public string[] AccountStatuses { get; set; } = new string[5];
    public decimal CreditLimit { get; set; }
    public decimal CashLimit { get; set; }
    public decimal CreditBalance { get; set; }
    public decimal CashBalance { get; set; }
    public int ApprovedAuthCount { get; set; }
    public int DeclinedAuthCount { get; set; }
    public decimal ApprovedAuthAmount { get; set; }
    public decimal DeclinedAuthAmount { get; set; }

    public bool HasAvailableCredit(decimal requestedAmount)
        => CreditBalance + requestedAmount <= CreditLimit;

    public bool HasAvailableCash(decimal requestedAmount)
        => CashBalance + requestedAmount <= CashLimit;
}
```

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.PendingAuthorizationSummary (
    AccountId               BIGINT          NOT NULL,
    CustomerId              BIGINT          NOT NULL,
    AuthorizationStatus     NVARCHAR(20)    NOT NULL,
    AccountStatus1          CHAR(2)         NULL,
    AccountStatus2          CHAR(2)         NULL,
    AccountStatus3          CHAR(2)         NULL,
    AccountStatus4          CHAR(2)         NULL,
    AccountStatus5          CHAR(2)         NULL,
    CreditLimit             DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    CashLimit               DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    CreditBalance           DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    CashBalance             DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    ApprovedAuthCount       INT             NOT NULL DEFAULT 0,
    DeclinedAuthCount       INT             NOT NULL DEFAULT 0,
    ApprovedAuthAmount      DECIMAL(11,2)   NOT NULL DEFAULT 0.00,
    DeclinedAuthAmount      DECIMAL(11,2)   NOT NULL DEFAULT 0.00,

    -- Audit columns
    CreatedAt               DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt               DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    RowVersion              ROWVERSION      NOT NULL,

    CONSTRAINT PK_PendingAuthSummary PRIMARY KEY CLUSTERED (AccountId),
    CONSTRAINT CK_PendingAuthSummary_CreditLimit CHECK (CreditLimit >= 0),
    CONSTRAINT CK_PendingAuthSummary_CashLimit CHECK (CashLimit >= 0),
    CONSTRAINT CK_PendingAuthSummary_CashWithinCredit CHECK (CashLimit <= CreditLimit)
);

-- Index for customer-based lookups
CREATE NONCLUSTERED INDEX IX_PendingAuthSummary_CustomerId
    ON dbo.PendingAuthorizationSummary (CustomerId);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `PA-ACCT-ID` | `S9(11) COMP-3` | `BIGINT` | `long` | Unpack COMP-3, 6 bytes -> 11 digits |
| `PA-CUST-ID` | `9(09)` | `BIGINT` | `long` | Zoned decimal, 9 bytes |
| `PA-AUTH-STATUS` | `X(01)` | `NVARCHAR(20)` | `string` | Map single char to descriptive name |
| `PA-ACCOUNT-STATUS` | `X(02) OCCURS 5` | `CHAR(2)` x 5 | `string[]` | Split array into individual columns |
| Financial fields | `S9(09)V99 COMP-3` | `DECIMAL(11,2)` | `decimal` | Unpack COMP-3, implied V99 |
| Counter fields | `S9(04) COMP` | `INT` | `int` | Binary halfword (2 bytes) |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS SummaryCount FROM dbo.PendingAuthorizationSummary;

-- Financial field checksums
SELECT
    SUM(CAST(CreditLimit AS FLOAT)) AS TotalCreditLimit,
    SUM(CAST(CashLimit AS FLOAT)) AS TotalCashLimit,
    SUM(CAST(CreditBalance AS FLOAT)) AS TotalCreditBalance,
    SUM(CAST(CashBalance AS FLOAT)) AS TotalCashBalance,
    SUM(CAST(ApprovedAuthAmount AS FLOAT)) AS TotalApprovedAmt,
    SUM(CAST(DeclinedAuthAmount AS FLOAT)) AS TotalDeclinedAmt
FROM dbo.PendingAuthorizationSummary;

-- Counter totals
SELECT
    SUM(ApprovedAuthCount) AS TotalApproved,
    SUM(DeclinedAuthCount) AS TotalDeclined
FROM dbo.PendingAuthorizationSummary;

-- Verify cash limit <= credit limit
SELECT COUNT(*) AS ViolationCount
FROM dbo.PendingAuthorizationSummary
WHERE CashLimit > CreditLimit;

-- Verify no orphaned customer references
SELECT s.AccountId
FROM dbo.PendingAuthorizationSummary s
LEFT JOIN dbo.Customer c ON s.CustomerId = c.CustomerId
WHERE c.CustomerId IS NULL;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 4 | Risk management -- credit limit controls must be enforced in real-time. | The summary record is the control point for authorization decisions. Credit/cash limits and balances must be accurate and consistent. The `RowVersion` column enables optimistic concurrency to prevent race conditions during concurrent authorization processing. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- all changes to credit limits must be audited. | Credit and cash limit changes must be logged in an audit table with before/after values, timestamp, and user identity. |
| **PSD2** Art. 97 | Strong Customer Authentication for account access and payments. | The authorization status field gates whether an account can receive new authorizations. SCA requirements must be evaluated before updating this field. |
| **AML/KYC** (FFFS 2017:11) | Ongoing monitoring -- unusual authorization patterns. | High declined authorization counts (`DeclinedAuthCount`) or rapidly increasing balances may indicate suspicious activity. These fields feed the AML monitoring pipeline. |
| **DORA** Art. 11 | ICT data integrity -- financial data must be accurate. | COMP-3 unpacking must be validated with zero tolerance. Parallel-run comparison must confirm that authorization decisions (approve/decline) are identical between COBOL and .NET for the same input data. The `RowVersion` supports data integrity under concurrent access. |
| **DORA** Art. 9 | ICT risk management -- availability of critical functions. | The authorization summary is read on every authorization request. The Azure SQL table must meet the same availability SLA as the IMS database. Consider read replicas for high-throughput scenarios. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
