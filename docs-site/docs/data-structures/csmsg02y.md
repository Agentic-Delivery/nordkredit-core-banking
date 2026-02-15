---
id: DS-MSG2-001
title: "Abend/Error Message Structure"
copybook_name: "CSMSG02Y.cpy"
domain: "system"
used_by_programs: [COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (error handling)"
sidebar_position: 28
---

# DS-MSG2-001: Abend/Error Message Structure (CSMSG02Y)

## Overview

The `CSMSG02Y.cpy` copybook defines the **Abend/Error Message Structure**, a standardized data area used by all CICS programs to capture and format error information when an abnormal termination (abend) or unexpected error occurs. This structure provides a consistent error reporting format across the entire application.

In CICS terminology, an "abend" (abnormal end) is an unhandled error that terminates the current transaction. The CardDemo programs use this structure to capture error context before displaying an error message to the user or writing to the error log.

This is not a database record -- it is an in-memory working storage structure used during error handling.

**Source file:** `CSMSG02Y.cpy`
**Used by:** ALL CICS programs (error handling routines)

## Source COBOL

```cobol
01  ABEND-DATA.
  05  ABEND-CODE                            PIC X(4) VALUE SPACES.
  05  ABEND-CULPRIT                         PIC X(8) VALUE SPACES.
  05  ABEND-REASON                          PIC X(50) VALUE SPACES.
  05  ABEND-MSG                             PIC X(72) VALUE SPACES.
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `ABEND-CODE` | `X(4)` | 4 | Alphanumeric | CICS abend code (e.g., `ASRA`, `AEI0`, `PGMX`) |
| 2 | `ABEND-CULPRIT` | `X(8)` | 8 | Alphanumeric | Program name where the error occurred |
| 3 | `ABEND-REASON` | `X(50)` | 50 | Alphanumeric | Human-readable description of the error |
| 4 | `ABEND-MSG` | `X(72)` | 72 | Alphanumeric | Formatted error message for display or logging |

**Total structure length:** 134 bytes

## Field Notes

1. **ABEND-CODE** (`PIC X(4)`): Stores the CICS abend code. Common codes include:
   - `ASRA` -- Program check (data exception, addressing error)
   - `AEI0` -- Invalid CICS command
   - `PGMX` -- Program not found
   - `AEIP` -- CICS resource error
   - Application-defined codes (e.g., `USR1`, `DBER`)

   In .NET, this maps to exception types or custom error codes.

2. **ABEND-CULPRIT** (`PIC X(8)`): The 8-character CICS program name where the error originated. CICS program names are limited to 8 characters. In .NET, this is replaced by the full class name, method name, and stack trace captured in structured logging.

3. **ABEND-REASON** (`PIC X(50)`): A brief text description of what went wrong. Populated by the program's error handling routine. Examples: `"VSAM READ ERROR ON ACCTDATA"`, `"INVALID CUSTOMER ID"`, `"DB2 SQL ERROR -805"`. In .NET, this maps to the exception message.

4. **ABEND-MSG** (`PIC X(72)`): A formatted composite message, typically constructed by concatenating the code, culprit, and reason. Limited to 72 characters (one line on a 3270 screen). In .NET, structured logging removes this length constraint.

5. **VALUE SPACES initialization:** All fields are initialized to spaces, which is the COBOL convention for "no error." A program checks for a non-spaces `ABEND-CODE` to determine if an error has occurred.

6. **Error handling pattern:** The typical COBOL error handling flow is:
   ```cobol
   EXEC CICS HANDLE ABEND LABEL(ERROR-ROUTINE)
   ...
   ERROR-ROUTINE.
       MOVE EIBRESP  TO ABEND-CODE
       MOVE 'COACTUPC' TO ABEND-CULPRIT
       MOVE 'VSAM READ ERROR ON ACCTDATA' TO ABEND-REASON
       STRING ABEND-CODE ' IN ' ABEND-CULPRIT ': ' ABEND-REASON
           DELIMITED BY SIZE INTO ABEND-MSG
       EXEC CICS SEND TEXT FROM(ABEND-MSG) END-EXEC
   ```

## Target Architecture Mapping

| Aspect | CICS (Current) | .NET (Target) |
|--------|----------------|---------------|
| **Error data structure** | `ABEND-DATA` working storage | `Exception` class hierarchy + structured logging |
| **Error codes** | CICS 4-character abend codes | .NET exception types, HTTP status codes, custom error codes |
| **Error source** | `ABEND-CULPRIT` (8-char program name) | Stack trace, `nameof()`, `CallerMemberName` attribute |
| **Error description** | `ABEND-REASON` (50-char text) | `Exception.Message` (unlimited length) |
| **Error display** | `ABEND-MSG` sent to 3270 screen | Problem Details (RFC 7807) JSON response, error page, or toast notification |
| **Error logging** | Limited (CICS transient data queues) | Application Insights, structured logging with Serilog/NLog |

### .NET Error Handling (Conceptual)

```csharp
// Structured error for API responses (RFC 7807)
public class CardDemoProblemDetails : ProblemDetails
{
    public string ErrorCode { get; set; }    // Maps to ABEND-CODE
    public string SourceComponent { get; set; } // Maps to ABEND-CULPRIT
}

// Global exception handler middleware
public class GlobalExceptionHandler : IExceptionHandler
{
    private readonly ILogger<GlobalExceptionHandler> _logger;

    public async ValueTask<bool> TryHandleAsync(
        HttpContext context,
        Exception exception,
        CancellationToken ct)
    {
        _logger.LogError(exception,
            "Unhandled exception in {Component}: {Message}",
            exception.TargetSite?.DeclaringType?.Name,
            exception.Message);

        var problemDetails = new CardDemoProblemDetails
        {
            Status = StatusCodes.Status500InternalServerError,
            Title = "An internal error occurred",
            ErrorCode = MapToErrorCode(exception),
            SourceComponent = exception.TargetSite?.DeclaringType?.Name
        };

        context.Response.StatusCode = problemDetails.Status.Value;
        await context.Response.WriteAsJsonAsync(problemDetails, ct);
        return true;
    }
}
```

## Migration Notes

1. **Replace with structured exceptions:** The flat 4-field `ABEND-DATA` structure is replaced by .NET's rich exception hierarchy. Define domain-specific exception classes (e.g., `AccountNotFoundException`, `TransactionProcessingException`) that carry structured error information including error codes, affected entity IDs, and contextual data.

2. **Implement global error handling:** CICS programs each implement their own error handling routine. In .NET, use global exception handling middleware (`IExceptionHandler` in .NET 8) to catch and log unhandled exceptions consistently across all endpoints.

3. **Structured logging:** Replace the 72-character `ABEND-MSG` with structured logging via Application Insights. Log entries should include:
   - Exception type and message
   - Stack trace
   - Correlation ID (for request tracing)
   - User context (sanitized -- no PII in logs)
   - Affected entity IDs

4. **RFC 7807 Problem Details:** API error responses should follow the RFC 7807 Problem Details standard, providing machine-readable error information to API consumers.

5. **Do not expose internal details:** The COBOL system displays abend codes and program names to the user. The .NET system must not expose internal error details (stack traces, SQL errors, component names) to end users. Return user-friendly error messages while logging full technical details server-side.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- error monitoring and incident management | All application errors must be logged with sufficient detail for investigation. The .NET system must implement centralized error logging via Application Insights with alerting thresholds for error rates. |
| **DORA** Art. 10 | ICT incident detection and reporting | Error patterns must be monitored for potential ICT incidents. Repeated errors of the same type may indicate a systematic issue requiring incident escalation. Application Insights anomaly detection should be configured. |
| **DORA** Art. 17 | Major ICT incident reporting | Critical errors (e.g., data corruption, service unavailability) must be classifiable as major ICT incidents for regulatory reporting. Error classification logic should categorize errors by severity. |
| **GDPR** Art. 33 | Notification of personal data breach | Errors involving unauthorized data access or data corruption must trigger breach notification workflows. Error logs must not contain PII (customer names, card numbers, SSNs) -- use anonymized identifiers only. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
