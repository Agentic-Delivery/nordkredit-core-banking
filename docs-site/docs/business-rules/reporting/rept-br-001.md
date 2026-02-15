---
id: "rept-br-001"
title: "Transaction report type selection"
domain: "reporting"
cobol_source: "CORPT00C.cbl:208-443"
requirement_id: "REPT-BR-001"
regulations:
  - "FSA FFFS 2014:5 Ch. 8 §4 (reporting controls)"
  - "DORA Art. 11 (ICT incident reporting)"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# REPT-BR-001: Transaction report type selection

## Summary

The report generation screen (CORPT0A) allows users to select one of three mutually exclusive report types for transaction reporting: Monthly, Yearly, or Custom. Monthly auto-calculates the current month's date range from the first day to the last day of the month. Yearly auto-calculates the full calendar year range from January 1 to December 31. Custom requires the user to manually enter start and end dates. If no report type is selected, an error message is displayed. Only one option may be active at a time, enforcing a single-selection constraint before report generation can proceed.

## Business Logic

### Pseudocode

```
FUNCTION SelectReportType(selection):
    IF selection = "Monthly" THEN
        currentDate = GET_CURRENT_DATE()
        startDate = currentDate.Year + "-" + currentDate.Month + "-01"
        nextMonth = currentDate.Month + 1
        IF nextMonth > 12 THEN
            nextMonth = 1
            year = currentDate.Year + 1
        ELSE
            year = currentDate.Year
        END IF
        endDate = DATE(year, nextMonth, 1) - 1 DAY
        RETURN (startDate, endDate)

    ELSE IF selection = "Yearly" THEN
        currentDate = GET_CURRENT_DATE()
        startDate = currentDate.Year + "-01-01"
        endDate = currentDate.Year + "-12-31"
        RETURN (startDate, endDate)

    ELSE IF selection = "Custom" THEN
        PROMPT user for startDate and endDate
        VALIDATE custom dates (see REPT-BR-002)
        RETURN (startDate, endDate)

    ELSE
        DISPLAY error "Please select a report type"
        RETURN ERROR
    END IF
END FUNCTION
```

### Decision Table

| Selection | Start Date Calculation | End Date Calculation | User Input Required | Error |
|-----------|----------------------|---------------------|---------------------|-------|
| Monthly | First day of current month (YYYY-MM-01) | Last day of current month (next month 1st - 1 day) | No | No |
| Yearly | January 1 of current year (YYYY-01-01) | December 31 of current year (YYYY-12-31) | No | No |
| Custom | User-provided start date | User-provided end date | Yes | No (if valid) |
| None selected | N/A | N/A | N/A | "Please select a report type" |

## Source COBOL Reference

**Program:** `CORPT00C.cbl`
**Lines:** 208-443 (Report type selection and date range calculation)

Monthly report date calculation (lines 213-238):
```cobol
000213     EVALUATE TRUE
000214         WHEN MONTHLYI OF CORPT0AI NOT = SPACES
000215              AND MONTHLYI OF CORPT0AI NOT = LOW-VALUES
000216             MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
000217             MOVE WS-CURRENT-YEAR  TO WS-START-YEAR
000218             MOVE WS-CURRENT-MONTH TO WS-START-MONTH
000219             MOVE 01               TO WS-START-DAY
000220             STRING WS-START-YEAR  '-'
000221                    WS-START-MONTH '-'
000222                    WS-START-DAY
000223                    DELIMITED BY SIZE INTO WS-START-DATE
000224             ADD 1 TO WS-CURRENT-MONTH
000225             IF WS-CURRENT-MONTH > 12
000226                 MOVE 1 TO WS-CURRENT-MONTH
000227                 ADD 1 TO WS-CURRENT-YEAR
000228             END-IF
000229             MOVE WS-CURRENT-YEAR  TO WS-END-YEAR
000230             MOVE WS-CURRENT-MONTH TO WS-END-MONTH
000231             MOVE 01               TO WS-END-DAY
000232             COMPUTE WS-END-DATE-INT =
000233                 FUNCTION INTEGER-OF-DATE(
000234                     WS-END-YEAR * 10000 +
000235                     WS-END-MONTH * 100 + WS-END-DAY) - 1
000236             MOVE FUNCTION DATE-OF-INTEGER(WS-END-DATE-INT)
000237                 TO WS-END-DATE
000238         WHEN YEARLYI OF CORPT0AI NOT = SPACES
```

Yearly report date calculation (lines 239-255):
```cobol
000239              AND YEARLYI OF CORPT0AI NOT = LOW-VALUES
000240             MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
000241             MOVE WS-CURRENT-YEAR TO WS-START-YEAR
000242             MOVE '01'            TO WS-START-MONTH
000243             MOVE '01'            TO WS-START-DAY
000244             STRING WS-START-YEAR  '-'
000245                    WS-START-MONTH '-'
000246                    WS-START-DAY
000247                    DELIMITED BY SIZE INTO WS-START-DATE
000248             MOVE WS-CURRENT-YEAR TO WS-END-YEAR
000249             MOVE '12'            TO WS-END-MONTH
000250             MOVE '31'            TO WS-END-DAY
000251             STRING WS-END-YEAR  '-'
000252                    WS-END-MONTH '-'
000253                    WS-END-DAY
000254                    DELIMITED BY SIZE INTO WS-END-DATE
000255         WHEN CUSTOMI OF CORPT0AI NOT = SPACES
```

No selection error handling (lines 437-443):
```cobol
000437     IF WS-REPORT-TYPE = SPACES
000438         MOVE 'Please select a report type'
000439             TO WS-ERROR-MSG
000440         MOVE -1 TO MONTHLYL OF CORPT0AI
000441         MOVE 'N' TO WS-VALID-INPUT
000442         GO TO SEND-REPORT-SCREEN
000443     END-IF
```

## Acceptance Criteria

### Scenario 1: Monthly report type selected

```gherkin
Given the user is on the transaction report screen
When the user selects the "Monthly" report type
Then the start date is set to the first day of the current month
And the end date is set to the last day of the current month
And no manual date entry is required
```

### Scenario 2: Yearly report type selected

```gherkin
Given the user is on the transaction report screen
When the user selects the "Yearly" report type
Then the start date is set to January 1 of the current year
And the end date is set to December 31 of the current year
And no manual date entry is required
```

### Scenario 3: Custom report type selected

```gherkin
Given the user is on the transaction report screen
When the user selects the "Custom" report type
Then the user is prompted to enter a start date and end date
And the entered dates are validated per REPT-BR-002
```

### Scenario 4: No report type selected

```gherkin
Given the user is on the transaction report screen
When the user does not select any report type
And the user attempts to generate the report
Then the system displays the error message "Please select a report type"
And the cursor is positioned on the Monthly selection field
```

### Scenario 5: Only one report type active at a time

```gherkin
Given the user is on the transaction report screen
When the user selects the "Monthly" report type
Then the "Yearly" and "Custom" options are not active
And only the Monthly date range is calculated
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 8 §4 | Financial institutions must have adequate internal controls for reporting processes | Report type selection enforces structured, controlled report generation with validated date ranges, preventing ad-hoc or erroneous report parameters |
| DORA | Art. 11 | ICT-related incident reporting and operational resilience monitoring | Standardized report type selection ensures consistent reporting capability for operational monitoring and incident-related transaction analysis |

## Edge Cases

1. **Month boundary for monthly reports**: When the current month is December (month 12), the last-day calculation must roll over to January of the next year before subtracting one day to get December 31.
2. **Leap year handling**: Monthly reports for February must correctly calculate the last day as either the 28th or 29th depending on whether the current year is a leap year.
3. **LOW-VALUES vs SPACES**: The COBOL program checks for both LOW-VALUES and SPACES when detecting empty selections; the .NET implementation must handle null, empty string, and whitespace equivalently.
4. **Concurrent selection attempt**: If multiple report type fields contain values due to screen data remnants, the EVALUATE TRUE structure processes only the first matching condition (Monthly takes precedence over Yearly, which takes precedence over Custom).
5. **Year boundary for yearly reports**: The yearly report always uses the current year from FUNCTION CURRENT-DATE; if run on January 1, it generates a report for the new year (which may have no data yet).

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The monthly last-day-of-month calculation uses integer date arithmetic (go to next month's first day and subtract 1), which is a common COBOL pattern. Verify this logic is preserved exactly in the .NET implementation, especially for February in leap years.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
