---
id: DS-AUTH-ERR-001
title: "Authorization Error Log"
copybook_name: "CCPAUERY.cpy"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 122
status: "extracted"
target_schema: "dbo.AuthorizationErrorLog"
sidebar_position: 36
---

# DS-AUTH-ERR-001: Authorization Error Log (CCPAUERY)

## Overview

The `CCPAUERY.cpy` copybook defines the **Authorization Error Log Record**, the standard error logging structure for the authorization subsystem. This record captures error events generated during IMS-based card authorization processing, including the originating program, error severity level, subsystem identifier, error codes, and a descriptive message.

This error log is critical for audit trails and operational monitoring. The ERR-LEVEL field maps directly to .NET log levels, and the ERR-SUBSYSTEM field identifies which middleware layer (CICS, IMS, Db2, MQ, or application) generated the error.

**Source file:** `CCPAUERY.cpy` (from `app-authorization-ims-db2-mq/cpy/`)
**Record length:** 122 bytes
**Used by:** `CCPAUTH1` (IMS authorization program)

## Source COBOL

```cobol
01 ERROR-LOG-RECORD.
     05 ERR-DATE                     PIC  X(06).
     05 ERR-TIME                     PIC  X(06).
     05 ERR-APPLICATION              PIC  X(08).
     05 ERR-PROGRAM                  PIC  X(08).
     05 ERR-LOCATION                 PIC  X(04).
     05 ERR-LEVEL                    PIC  X(01).
        88 ERR-LOG                   VALUE 'L'.
        88 ERR-INFO                  VALUE 'I'.
        88 ERR-WARNING               VALUE 'W'.
        88 ERR-CRITICAL              VALUE 'C'.
     05 ERR-SUBSYSTEM                PIC  X(01).
        88 ERR-SUBSYSTEM-APP         VALUE 'A'.
        88 ERR-SUBSYSTEM-CICS        VALUE 'C'.
        88 ERR-SUBSYSTEM-IMS         VALUE 'I'.
        88 ERR-SUBSYSTEM-DB2         VALUE 'D'.
        88 ERR-SUBSYSTEM-MQ          VALUE 'M'.
        88 ERR-SUBSYSTEM-FILE        VALUE 'F'.
     05 ERR-CODE-1                   PIC  X(09).
     05 ERR-CODE-2                   PIC  X(09).
     05 ERR-MESSAGE                  PIC  X(50).
     05 ERR-EVENT-KEY                PIC  X(20).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Target Column |
|---|------------|-----------|--------|--------|------|-------------|---------------|
| 1 | `ERR-DATE` | `X(06)` | 0 | 6 | Alphanumeric | Error date (`YYMMDD` format) | `ErrorTimestamp` (DATETIME2, combined with ERR-TIME) |
| 2 | `ERR-TIME` | `X(06)` | 6 | 6 | Alphanumeric | Error time (`HHMMSS` format) | `ErrorTimestamp` (DATETIME2, combined with ERR-DATE) |
| 3 | `ERR-APPLICATION` | `X(08)` | 12 | 8 | Alphanumeric | Application identifier (e.g., `CARDAUTH`) | `Application` (NVARCHAR(8)) |
| 4 | `ERR-PROGRAM` | `X(08)` | 20 | 8 | Alphanumeric | Program name that generated the error | `ProgramName` (NVARCHAR(8)) |
| 5 | `ERR-LOCATION` | `X(04)` | 28 | 4 | Alphanumeric | Location code within the program | `LocationCode` (NVARCHAR(4)) |
| 6 | `ERR-LEVEL` | `X(01)` | 32 | 1 | Alphanumeric | Error severity level | `LogLevel` (NVARCHAR(20)) |
| 7 | `ERR-SUBSYSTEM` | `X(01)` | 33 | 1 | Alphanumeric | Originating subsystem | `Subsystem` (NVARCHAR(20)) |
| 8 | `ERR-CODE-1` | `X(09)` | 34 | 9 | Alphanumeric | Primary error/return code | `ErrorCode1` (NVARCHAR(9)) |
| 9 | `ERR-CODE-2` | `X(09)` | 43 | 9 | Alphanumeric | Secondary error/reason code | `ErrorCode2` (NVARCHAR(9)) |
| 10 | `ERR-MESSAGE` | `X(50)` | 52 | 50 | Alphanumeric | Descriptive error message | `Message` (NVARCHAR(200)) |
| 11 | `ERR-EVENT-KEY` | `X(20)` | 102 | 20 | Alphanumeric | Key linking error to triggering event | `EventKey` (NVARCHAR(20)) |

**Total record length:** 122 bytes

## Field Notes

1. **ERR-DATE** (`PIC X(06)`) -- Date in `YYMMDD` format (6 characters, no separators). Note the 2-digit year, which requires century assumption during migration (all dates are assumed to be in the 2000s for recent data, 1900s for historical). Combined with `ERR-TIME` to form a single `DATETIME2` column in the target.

2. **ERR-TIME** (`PIC X(06)`) -- Time in `HHMMSS` format (6 characters, no separators, 24-hour clock). Combined with `ERR-DATE` for the target timestamp.

3. **ERR-APPLICATION** (`PIC X(08)`) -- Identifies the application context (e.g., `CARDAUTH` for the card authorization subsystem). Right-padded with spaces.

4. **ERR-PROGRAM** (`PIC X(08)`) -- The specific COBOL program that generated the error (e.g., `CCPAUTH1`). Useful for tracing errors back to source code.

5. **ERR-LOCATION** (`PIC X(04)`) -- A short code identifying the location within the program where the error occurred (e.g., a paragraph number or section identifier). Equivalent to a method/line reference in .NET.

6. **ERR-LEVEL** (`PIC X(01)`) -- Severity level with 88-level conditions:

   | COBOL Value | 88-Level Name | .NET LogLevel | Description |
   |-------------|--------------|---------------|-------------|
   | `L` | `ERR-LOG` | `Debug` / `Trace` | General log entry |
   | `I` | `ERR-INFO` | `Information` | Informational message |
   | `W` | `ERR-WARNING` | `Warning` | Warning condition |
   | `C` | `ERR-CRITICAL` | `Critical` / `Error` | Critical error requiring attention |

7. **ERR-SUBSYSTEM** (`PIC X(01)`) -- Identifies which middleware or infrastructure layer generated the error:

   | COBOL Value | 88-Level Name | Subsystem | .NET Equivalent |
   |-------------|--------------|-----------|-----------------|
   | `A` | `ERR-SUBSYSTEM-APP` | Application logic | Application code exception |
   | `C` | `ERR-SUBSYSTEM-CICS` | CICS transaction manager | ASP.NET middleware |
   | `I` | `ERR-SUBSYSTEM-IMS` | IMS database manager | Entity Framework / repository layer |
   | `D` | `ERR-SUBSYSTEM-DB2` | Db2 database | Azure SQL Database |
   | `M` | `ERR-SUBSYSTEM-MQ` | IBM MQ messaging | Azure Service Bus |
   | `F` | `ERR-SUBSYSTEM-FILE` | VSAM file I/O | Azure Blob Storage / file operations |

8. **ERR-CODE-1** (`PIC X(09)`) -- Primary error code. For Db2 errors, this contains the SQLCODE. For IMS errors, this contains the PCB status code. For MQ errors, this contains the MQ reason code.

9. **ERR-CODE-2** (`PIC X(09)`) -- Secondary error code providing additional detail. For Db2, this may contain the SQLSTATE. For IMS, this may contain the segment name.

10. **ERR-MESSAGE** (`PIC X(50)`) -- Human-readable error description. Limited to 50 characters in COBOL. The target column is widened to `NVARCHAR(200)` to allow more descriptive messages in the .NET system.

11. **ERR-EVENT-KEY** (`PIC X(20)`) -- Links the error log entry to the business event (e.g., authorization request) that triggered it. This enables correlation between error logs and transaction records.

## Target Architecture Mapping

| Aspect | COBOL/IMS (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Error logging** | Write to error log file/queue | Structured logging via `ILogger` + Application Insights |
| **Log levels** | Single-character codes (L/I/W/C) | `Microsoft.Extensions.Logging.LogLevel` enum |
| **Subsystem ID** | Single-character codes (A/C/I/D/M/F) | Structured log properties (`SourceContext`, custom dimensions) |
| **Error codes** | Free-text PIC X fields | Structured exception types + inner exception details |
| **Correlation** | `ERR-EVENT-KEY` | Distributed tracing (`Activity.Id`, correlation ID header) |
| **Storage** | Sequential file or IMS message queue | Application Insights + Azure SQL (for audit) |

### .NET Logging Equivalent

```csharp
// Structured logging replaces the flat error log record
public class AuthorizationErrorLogger
{
    private readonly ILogger<AuthorizationErrorLogger> _logger;

    public void LogAuthorizationError(
        string application,
        string programName,
        string locationCode,
        string subsystem,
        string errorCode1,
        string errorCode2,
        string message,
        string eventKey)
    {
        _logger.LogError(
            "Authorization error in {Application}/{Program} at {Location}. " +
            "Subsystem: {Subsystem}, Codes: {Code1}/{Code2}, Event: {EventKey}. {Message}",
            application, programName, locationCode,
            subsystem, errorCode1, errorCode2, eventKey, message);
    }
}
```

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.AuthorizationErrorLog (
    ErrorLogId          BIGINT IDENTITY(1,1)  NOT NULL,
    ErrorTimestamp       DATETIME2             NOT NULL,
    Application         NVARCHAR(8)           NOT NULL,
    ProgramName         NVARCHAR(8)           NOT NULL,
    LocationCode        NVARCHAR(4)           NULL,
    LogLevel            NVARCHAR(20)          NOT NULL,
    Subsystem           NVARCHAR(20)          NOT NULL,
    ErrorCode1          NVARCHAR(9)           NULL,
    ErrorCode2          NVARCHAR(9)           NULL,
    Message             NVARCHAR(200)         NOT NULL,
    EventKey            NVARCHAR(20)          NULL,

    -- Audit columns
    CreatedAt           DATETIME2             NOT NULL DEFAULT SYSUTCDATETIME(),

    CONSTRAINT PK_AuthorizationErrorLog PRIMARY KEY CLUSTERED (ErrorLogId)
);

-- Index for time-based queries (recent errors)
CREATE NONCLUSTERED INDEX IX_AuthErrorLog_Timestamp
    ON dbo.AuthorizationErrorLog (ErrorTimestamp DESC);

-- Index for event correlation
CREATE NONCLUSTERED INDEX IX_AuthErrorLog_EventKey
    ON dbo.AuthorizationErrorLog (EventKey)
    WHERE EventKey IS NOT NULL;

-- Index for severity-based alerting
CREATE NONCLUSTERED INDEX IX_AuthErrorLog_Level
    ON dbo.AuthorizationErrorLog (LogLevel, ErrorTimestamp DESC);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `ERR-DATE` + `ERR-TIME` | `X(06)` + `X(06)` | `DATETIME2` | `DateTime` | Combined into single timestamp |
| `ERR-APPLICATION` | `X(08)` | `NVARCHAR(8)` | `string` | Trimmed |
| `ERR-PROGRAM` | `X(08)` | `NVARCHAR(8)` | `string` | Trimmed |
| `ERR-LOCATION` | `X(04)` | `NVARCHAR(4)` | `string` | Trimmed |
| `ERR-LEVEL` | `X(01)` | `NVARCHAR(20)` | `LogLevel` (enum) | Mapped from single char to enum |
| `ERR-SUBSYSTEM` | `X(01)` | `NVARCHAR(20)` | `string` (enum) | Mapped from single char to descriptive name |
| `ERR-CODE-1` | `X(09)` | `NVARCHAR(9)` | `string` | Trimmed |
| `ERR-CODE-2` | `X(09)` | `NVARCHAR(9)` | `string` | Trimmed |
| `ERR-MESSAGE` | `X(50)` | `NVARCHAR(200)` | `string` | Widened for richer messages |
| `ERR-EVENT-KEY` | `X(20)` | `NVARCHAR(20)` | `string` | Correlation key |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS ErrorLogCount FROM dbo.AuthorizationErrorLog;

-- Verify log level distribution
SELECT LogLevel, COUNT(*) AS Cnt
FROM dbo.AuthorizationErrorLog
GROUP BY LogLevel;

-- Verify subsystem distribution
SELECT Subsystem, COUNT(*) AS Cnt
FROM dbo.AuthorizationErrorLog
GROUP BY Subsystem;

-- Check for critical errors without event keys
SELECT COUNT(*) AS OrphanedCritical
FROM dbo.AuthorizationErrorLog
WHERE LogLevel = 'Critical' AND EventKey IS NULL;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- all authorization errors must be logged for audit trail. Error logs must be retained for regulatory examination. | Error log records must be immutable (INSERT only, no UPDATE/DELETE). Retention period per FSA requirements (minimum 7 years for financial transaction-related errors). |
| **FSA (FFFS 2014:5)** Ch. 4 | Risk management -- critical errors must trigger alerts. | Implement Azure Monitor alerts on `LogLevel = 'Critical'` entries. Automated escalation for unresolved critical authorization errors. |
| **PSD2** Art. 96 | Incident reporting -- major operational incidents must be reported to the competent authority. | Critical authorization errors may indicate a major incident. Error logs feed into the incident detection pipeline. |
| **DORA** Art. 17 | ICT-related incident management -- financial entities must have processes for detecting and managing ICT incidents. | Error logs are a primary input to incident detection. The `ERR-SUBSYSTEM` field enables rapid identification of the affected infrastructure layer. |
| **DORA** Art. 12 | Logging -- financial entities must log all ICT-related activities. | The error log structure satisfies the requirement for structured logging of ICT events in the authorization subsystem. |
| **AML/KYC** | Authorization errors may indicate fraudulent activity patterns. | Error log analysis should be integrated with the AML monitoring pipeline. Patterns of repeated authorization failures for specific accounts or merchants must be flagged. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
