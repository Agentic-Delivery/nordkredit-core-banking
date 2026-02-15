---
id: "rept-br-002"
title: "Custom date range validation for transaction reports"
domain: "reporting"
cobol_source: "CORPT00C.cbl:258-426"
requirement_id: "REPT-BR-002"
regulations:
  - "FFFS 2014:5 Ch. 4 §3 (operational risk)"
  - "FSA reporting accuracy requirements"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# REPT-BR-002: Custom date range validation for transaction reports

## Summary

When the custom report type is selected, the system performs a sequential three-stage validation on user-provided start and end dates. First, each date component (month, day, year for both start and end dates) is checked for blank or LOW-VALUES entries. Second, numeric range validation ensures months are between 1-12, days are between 1-31, and years are valid numeric values, using the FUNCTION NUMVAL-C intrinsic for conversion. Third, the assembled dates are passed to an external utility program (CSUTLDTC) for full calendar validity checking, which returns a severity code where '0000' indicates a valid date. Validation failures at any stage halt processing and display a specific error message to the user.

## Business Logic

### Pseudocode

```
FUNCTION ValidateCustomDateRange(startMonth, startDay, startYear,
                                  endMonth, endDay, endYear):

    -- Stage 1: Empty field checks
    FOR EACH field IN [startMonth, startDay, startYear,
                       endMonth, endDay, endYear]:
        IF field IS BLANK OR field IS NULL THEN
            SET errorMessage = "Please enter complete date information"
            SET cursor to first empty field
            RETURN VALIDATION_ERROR
        END IF
    END FOR

    -- Stage 2: Numeric range validation
    startMonthNum = NUMVAL(startMonth)
    IF startMonthNum < 1 OR startMonthNum > 12 THEN
        SET errorMessage = "Start month must be 01-12"
        RETURN VALIDATION_ERROR
    END IF

    startDayNum = NUMVAL(startDay)
    IF startDayNum < 1 OR startDayNum > 31 THEN
        SET errorMessage = "Start day must be 01-31"
        RETURN VALIDATION_ERROR
    END IF

    startYearNum = NUMVAL(startYear)
    IF NOT NUMERIC(startYearNum) THEN
        SET errorMessage = "Start year must be numeric"
        RETURN VALIDATION_ERROR
    END IF

    endMonthNum = NUMVAL(endMonth)
    IF endMonthNum < 1 OR endMonthNum > 12 THEN
        SET errorMessage = "End month must be 01-12"
        RETURN VALIDATION_ERROR
    END IF

    endDayNum = NUMVAL(endDay)
    IF endDayNum < 1 OR endDayNum > 31 THEN
        SET errorMessage = "End day must be 01-31"
        RETURN VALIDATION_ERROR
    END IF

    endYearNum = NUMVAL(endYear)
    IF NOT NUMERIC(endYearNum) THEN
        SET errorMessage = "End year must be numeric"
        RETURN VALIDATION_ERROR
    END IF

    -- Stage 3: Full date validity via external utility
    assembledStartDate = FORMAT_DATE(startYear, startMonth, startDay)
    CALL CSUTLDTC WITH assembledStartDate
    IF severityCode != '0000' AND messageNumber != '2513' THEN
        SET errorMessage = "Start date is not a valid date"
        RETURN VALIDATION_ERROR
    END IF

    assembledEndDate = FORMAT_DATE(endYear, endMonth, endDay)
    CALL CSUTLDTC WITH assembledEndDate
    IF severityCode != '0000' AND messageNumber != '2513' THEN
        SET errorMessage = "End date is not a valid date"
        RETURN VALIDATION_ERROR
    END IF

    -- Stage 4: Logical range check
    IF assembledStartDate > assembledEndDate THEN
        SET errorMessage = "Start date must be before end date"
        RETURN VALIDATION_ERROR
    END IF

    RETURN VALIDATION_SUCCESS
END FUNCTION
```

### Decision Table

| Validation Stage | Condition | Result | Error Message |
|-----------------|-----------|--------|---------------|
| Stage 1: Empty check | Any date field is blank or LOW-VALUES | Fail | "Please enter complete date information" |
| Stage 2: Range check | Month &lt; 1 or Month &gt; 12 | Fail | "\[Start/End\] month must be 01-12" |
| Stage 2: Range check | Day &lt; 1 or Day &gt; 31 | Fail | "\[Start/End\] day must be 01-31" |
| Stage 2: Range check | Year is not numeric | Fail | "\[Start/End\] year must be numeric" |
| Stage 3: Date validity | CSUTLDTC severity != '0000' (and msg != '2513') | Fail | "\[Start/End\] date is not a valid date" |
| All stages pass | All fields valid | Success | N/A |

## Source COBOL Reference

**Program:** `CORPT00C.cbl`
**Lines:** 258-426 (Custom date range validation)

Empty field checks (lines 259-303):
```cobol
000259         IF SDTMMI OF CORPT0AI = SPACES
000260             OR SDTMMI OF CORPT0AI = LOW-VALUES
000261             MOVE 'Please enter start month'
000262                 TO WS-ERROR-MSG
000263             MOVE -1 TO SDTMML OF CORPT0AI
000264             MOVE 'N' TO WS-VALID-INPUT
000265             GO TO SEND-REPORT-SCREEN
000266         END-IF
000267         IF SDTDDI OF CORPT0AI = SPACES
000268             OR SDTDDI OF CORPT0AI = LOW-VALUES
000269             MOVE 'Please enter start day'
000270                 TO WS-ERROR-MSG
000271             MOVE -1 TO SDTDDL OF CORPT0AI
000272             MOVE 'N' TO WS-VALID-INPUT
000273             GO TO SEND-REPORT-SCREEN
000274         END-IF
000275         IF SDTYYI OF CORPT0AI = SPACES
000276             OR SDTYYI OF CORPT0AI = LOW-VALUES
000277             MOVE 'Please enter start year'
000278                 TO WS-ERROR-MSG
000279             MOVE -1 TO SDTYYL OF CORPT0AI
000280             MOVE 'N' TO WS-VALID-INPUT
000281             GO TO SEND-REPORT-SCREEN
000282         END-IF
```

Numeric range validation (lines 305-379):
```cobol
000305         COMPUTE WS-TEMP-NUM =
000306             FUNCTION NUMVAL-C(SDTMMI OF CORPT0AI)
000307         IF WS-TEMP-NUM < 1 OR WS-TEMP-NUM > 12
000308             MOVE 'Start month must be 01-12'
000309                 TO WS-ERROR-MSG
000310             MOVE -1 TO SDTMML OF CORPT0AI
000311             MOVE 'N' TO WS-VALID-INPUT
000312             GO TO SEND-REPORT-SCREEN
000313         END-IF
000314         COMPUTE WS-TEMP-NUM =
000315             FUNCTION NUMVAL-C(SDTDDI OF CORPT0AI)
000316         IF WS-TEMP-NUM < 1 OR WS-TEMP-NUM > 31
000317             MOVE 'Start day must be 01-31'
000318                 TO WS-ERROR-MSG
000319             MOVE -1 TO SDTDDL OF CORPT0AI
000320             MOVE 'N' TO WS-VALID-INPUT
000321             GO TO SEND-REPORT-SCREEN
000322         END-IF
```

Date validity check via external utility (lines 388-426):
```cobol
000388         STRING SDTYYI OF CORPT0AI
000389                SDTMMI OF CORPT0AI
000390                SDTDDI OF CORPT0AI
000391                DELIMITED BY SIZE INTO WS-DATE-TO-VALIDATE
000392         CALL 'CSUTLDTC' USING WS-DATE-TO-VALIDATE
000393                               WS-DATE-RESULT
000394         IF WS-SEVERITY-CODE NOT = '0000'
000395             AND WS-MESSAGE-NUMBER NOT = '2513'
000396             MOVE 'Start date is not a valid date'
000397                 TO WS-ERROR-MSG
000398             MOVE -1 TO SDTMML OF CORPT0AI
000399             MOVE 'N' TO WS-VALID-INPUT
000400             GO TO SEND-REPORT-SCREEN
000401         END-IF
```

## Acceptance Criteria

### Scenario 1: All date fields provided and valid

```gherkin
Given the user has selected the "Custom" report type
When the user enters start date "01/15/2025" and end date "03/31/2025"
And all fields are non-empty
And all numeric values are within range
And both dates are valid calendar dates
Then the date range is accepted for report generation
```

### Scenario 2: Empty start month field

```gherkin
Given the user has selected the "Custom" report type
When the user leaves the start month field blank
And attempts to generate the report
Then the system displays "Please enter start month"
And the cursor is positioned on the start month field
```

### Scenario 3: Month value out of range

```gherkin
Given the user has selected the "Custom" report type
When the user enters "13" as the start month
Then the system displays "Start month must be 01-12"
And the cursor is positioned on the start month field
```

### Scenario 4: Invalid calendar date

```gherkin
Given the user has selected the "Custom" report type
When the user enters start date "02/30/2025"
And the date components pass basic range checks
Then the external date validation utility rejects the date
And the system displays "Start date is not a valid date"
```

### Scenario 5: Start date after end date

```gherkin
Given the user has selected the "Custom" report type
When the user enters start date "06/15/2025" and end date "01/10/2025"
And both dates are individually valid
Then the system displays "Start date must be before end date"
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §3 | Financial institutions shall manage operational risks including errors in information systems | Multi-stage date validation prevents invalid report parameters from reaching the batch processing pipeline, reducing operational risk from erroneous report generation |
| FSA | Reporting accuracy requirements | Regulatory reports must be based on accurate and complete data | Rigorous date validation ensures report date ranges are valid calendar dates, preventing reports with nonsensical date boundaries that could produce inaccurate regulatory submissions |

## Edge Cases

1. **February 29 in non-leap years**: The basic range check allows day &lt;= 31, but February 30/31 and February 29 in non-leap years are only caught by the CSUTLDTC utility in Stage 3. The .NET implementation must replicate this two-phase validation behavior.
2. **NUMVAL-C with non-numeric input**: If the user enters alphabetic characters in a numeric field, FUNCTION NUMVAL-C may cause a runtime error in COBOL. The .NET implementation should handle non-numeric input gracefully with a clear error message.
3. **Message number 2513 exception**: The CSUTLDTC utility returns message '2513' for certain non-critical conditions that should not prevent date acceptance. The specific meaning of this message must be confirmed with the domain expert.
4. **LOW-VALUES vs empty string**: COBOL LOW-VALUES (hex 00) is distinct from SPACES. The .NET implementation must treat null, empty string, and whitespace-only strings as equivalent to both conditions.
5. **Two-digit vs four-digit year**: The COBOL screen fields suggest separate input for month, day, and year. Confirm whether year is expected as 2-digit or 4-digit and how century windowing is handled.
6. **Date field order**: Validation proceeds in fixed order (start month, start day, start year, end month, end day, end year). Only the first error is reported. The .NET implementation should preserve this sequential behavior rather than collecting all errors.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The CSUTLDTC utility message number '2513' exception needs clarification -- confirm what condition produces this message and why it is acceptable to ignore. Also verify whether any additional date range constraints exist (e.g., maximum span, future date restrictions).

---
**Template version:** 1.0
**Last updated:** 2026-02-15
