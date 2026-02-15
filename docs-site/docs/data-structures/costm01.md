---
id: DS-STMT-001
title: "Statement/Transaction Reporting Layout"
copybook_name: "COSTM01.CPY"
domain: "transactions"
used_by_programs: [CORPT00C, CBTRN04C]
record_length: 0
status: "extracted"
target_schema: "N/A (report view)"
sidebar_position: 23
---

# DS-STMT-001: Statement/Transaction Reporting Layout (COSTM01)

## Overview

The `COSTM01.CPY` copybook defines the **Statement/Transaction Reporting Layout**, a reorganized view of transaction data optimized for statement generation and card-level transaction reporting. The structure uses a composite key of card number + transaction ID, providing an access path for retrieving all transactions for a specific card in statement order.

This is not a separate database record -- it uses the same underlying data as the Transaction Record (`CVTRA05Y`) but with a different key structure and field organization suited for report generation. It is used by the online report program (`CORPT00C`) and the batch statement generation program (`CBTRN04C`).

**Source file:** `COSTM01.CPY`
**Used by:** `CORPT00C` (Report Request), `CBTRN04C` (Batch Statement Generation)

## Source COBOL

```cobol
01 TRNX-RECORD.
   05 TRNX-KEY.
      10 TRNX-CARD-NUM              PIC X(16).
      10 TRNX-ID                    PIC X(16).
   05 TRNX-REST.
      10 TRNX-TYPE-CD               PIC X(02).
      10 TRNX-CAT-CD                PIC 9(04).
      10 TRNX-SOURCE                PIC X(10).
      10 TRNX-DESC                  PIC X(100).
      10 TRNX-AMT                   PIC S9(09)V99.
      10 TRNX-MERCHANT-ID           PIC 9(09).
      10 TRNX-MERCHANT-NAME         PIC X(50).
      10 TRNX-MERCHANT-CITY         PIC X(50).
      10 TRNX-MERCHANT-ZIP          PIC X(10).
      10 TRNX-ORIG-TS               PIC X(26).
      10 TRNX-PROC-TS               PIC X(26).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `TRNX-CARD-NUM` | `X(16)` | 16 | Alphanumeric | Card number (part of composite key) |
| 2 | `TRNX-ID` | `X(16)` | 16 | Alphanumeric | Transaction identifier (part of composite key) |
| 3 | `TRNX-TYPE-CD` | `X(02)` | 2 | Alphanumeric | Transaction type code |
| 4 | `TRNX-CAT-CD` | `9(04)` | 4 | Numeric | Transaction category code |
| 5 | `TRNX-SOURCE` | `X(10)` | 10 | Alphanumeric | Transaction source/origin |
| 6 | `TRNX-DESC` | `X(100)` | 100 | Alphanumeric | Transaction description |
| 7 | `TRNX-AMT` | `S9(09)V99` | 11 | Signed numeric | Transaction amount with 2 decimal places |
| 8 | `TRNX-MERCHANT-ID` | `9(09)` | 9 | Numeric | Merchant identifier |
| 9 | `TRNX-MERCHANT-NAME` | `X(50)` | 50 | Alphanumeric | Merchant name |
| 10 | `TRNX-MERCHANT-CITY` | `X(50)` | 50 | Alphanumeric | Merchant city |
| 11 | `TRNX-MERCHANT-ZIP` | `X(10)` | 10 | Alphanumeric | Merchant ZIP/postal code |
| 12 | `TRNX-ORIG-TS` | `X(26)` | 26 | Alphanumeric | Transaction origination timestamp |
| 13 | `TRNX-PROC-TS` | `X(26)` | 26 | Alphanumeric | Transaction processing timestamp |

## Field Notes

1. **TRNX-KEY (Composite key):** The key is structured as card number + transaction ID, which is different from the primary Transaction Record (`CVTRA05Y`) that keys by transaction ID alone. This composite key supports efficient retrieval of all transactions for a specific card, which is the primary access pattern for statement generation.

2. **TRNX-CARD-NUM** (`PIC X(16)`): Note that this field uses `PIC X` (alphanumeric) rather than `PIC 9` (numeric), even though card numbers are numeric. This is because the field serves as part of a VSAM key, where alphanumeric comparison (left-to-right, byte-by-byte) is used for key sequencing.

3. **TRNX-AMT** (`PIC S9(09)V99`): Signed transaction amount with implied decimal. Positive values represent debits (charges), negative values represent credits (refunds/payments). Maximum value: +/- 999,999,999.99.

4. **TRNX-ORIG-TS / TRNX-PROC-TS** (`PIC X(26)`): Timestamp fields in ISO-like format (`YYYY-MM-DD-HH.MM.SS.SSSSSS`). The origination timestamp records when the transaction was initiated; the processing timestamp records when it was settled.

5. **Relationship to CVTRA05Y:** The fields in `TRNX-REST` are structurally identical to the corresponding fields in `CVTRA05Y` (Transaction Record). This copybook provides an alternative access path (by card number) to the same logical data. In the target SQL database, this is achieved with an index on `(CardNumber, TransactionId)` rather than a separate table.

## Target Architecture Mapping

| Aspect | CICS/Batch (Current) | .NET (Target) |
|--------|----------------------|---------------|
| **Data access** | VSAM alternate index on card number + transaction ID | SQL query with `WHERE CardNumber = @cardNum ORDER BY TransactionId` or indexed view |
| **Statement generation** | Batch program `CBTRN04C` reads VSAM sequentially by card | Azure Function with SQL query, grouped by card number |
| **Report display** | `CORPT00C` sends BMS screen with transaction list | Blazor/Razor page with paginated transaction list or PDF export |
| **Card number handling** | Full PAN in key and record | Tokenized card reference; PAN retrieved only for processing, never displayed in full |

### .NET Query Pattern (Conceptual)

```csharp
public class StatementQuery
{
    public string CardNumberToken { get; set; } // Tokenized reference
    public DateOnly StatementStartDate { get; set; }
    public DateOnly StatementEndDate { get; set; }
}

// Repository method
public async Task<IReadOnlyList<TransactionDto>> GetCardTransactionsAsync(
    string cardNumber,
    DateOnly from,
    DateOnly to,
    CancellationToken ct)
{
    return await _context.Transactions
        .Where(t => t.CardNumber == cardNumber
                  && t.OriginationTimestamp >= from.ToDateTime(TimeOnly.MinValue)
                  && t.OriginationTimestamp <= to.ToDateTime(TimeOnly.MaxValue))
        .OrderBy(t => t.TransactionId)
        .Select(t => new TransactionDto { /* ... */ })
        .ToListAsync(ct);
}
```

## Migration Notes

1. **No separate table needed:** In the VSAM world, the `COSTM01` copybook represents an alternate index path to the same transaction data. In Azure SQL, this is replaced by a covering index on the `Transaction` table: `CREATE INDEX IX_Transaction_CardNumber ON dbo.Transaction (CardNumber, TransactionId) INCLUDE (...)`.

2. **Statement batch job:** The batch program `CBTRN04C` that uses this copybook for statement generation maps to an Azure Function with a timer trigger. The SLA requirement (monthly statements complete by day 3) must be validated during parallel-run testing.

3. **Card number in reports:** Statement reports contain the full card number. In the .NET system, PCI-DSS requires masking the PAN on statements (show only last 4 digits). The card number in the composite key is used internally for data retrieval but must be masked in all output.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS** v4.0 Req. 3.4 | Render PAN unreadable in storage and display | Card number in the composite key must be tokenized or encrypted in the target system. Statement output must mask the PAN (e.g., `**** **** **** 1234`). |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- statement accuracy | Statement generation must produce identical results to the mainframe system during parallel-run. Financial totals must match to the cent. |
| **GDPR** Art. 15 | Right of access -- customer data portability | Customers may request transaction statements as part of a data access request. The .NET system should support automated statement generation for GDPR compliance. |
| **PSD2** Art. 57 | Information on individual payment transactions | Payment service providers must provide transaction details upon request. The statement layout must include all required PSD2 information fields (amount, date, merchant, reference). |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
