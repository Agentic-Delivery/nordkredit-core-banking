---
id: "acct-br-003"
title: "Account update field validation rules"
domain: "account-management"
cobol_source: "COACTUPC.cbl:1429-1676"
requirement_id: "ACCT-BR-003"
regulations:
  - "FFFS 2014:5 Ch. 4 S3"
  - "PSD2 Art. 97"
  - "GDPR Art. 5(1)(d)"
  - "AML/KYC"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-003: Account update field validation rules

## Summary

The COACTUPC program validates all user-entered fields before allowing account and customer data updates. This is the most complex validation logic in the Account Management domain, comprising 25+ field-level validations across account financial fields, customer personal data fields, and cross-field consistency checks. The validation is performed in a single pass through the edit paragraphs (lines 1429-1676 for the main orchestration, with individual edit routines spanning lines 1783-2560). Each field validation sets a specific error message and halts further processing on first error. Currency fields use the COBOL FUNCTION TEST-NUMVAL-C intrinsic to validate signed currency input. Date fields require CCYYMMDD format. Customer name and address fields enforce alpha-only or presence checks. SSN validation follows the Social Security Administration rules (no 000/666/900-999 in area number). FICO score must be in the 300-850 range. A state-ZIP cross-validation ensures geographic consistency. The currency display format is PIC +ZZZ,ZZZ,ZZZ.99 with internal storage as PIC S9(10)V99.

## Business Logic

### Pseudocode

```
PERFORM 1400-VALIDATE-ALL-FIELDS:

    -- Account Fields
    PERFORM 1410-EDIT-ACCOUNT-ID:
        Validate: 11-digit, non-zero, numeric (same as ACCT-BR-001 key rules)
        Error: 'ACCOUNT ID MUST BE 11 DIGIT NUMERIC'

    PERFORM 1420-EDIT-ACTIVE-STATUS:
        IF active-status NOT = 'Y' AND NOT = 'N'
            MOVE 'ACTIVE STATUS MUST BE Y OR N' TO WS-MESSAGE
            SET input-error = TRUE
        END-IF

    PERFORM 1430-EDIT-CREDIT-LIMIT:
        Validate: signed currency via TEST-NUMVAL-C
        COMPUTE test-result = FUNCTION TEST-NUMVAL-C(credit-limit-input)
        IF test-result > 0
            MOVE 'CREDIT LIMIT MUST BE A VALID CURRENCY AMOUNT' TO WS-MESSAGE
            SET input-error = TRUE
        ELSE
            COMPUTE credit-limit-numeric = FUNCTION NUMVAL-C(credit-limit-input)
        END-IF

    PERFORM 1440-EDIT-CASH-CREDIT-LIMIT:
        Same pattern as credit limit using TEST-NUMVAL-C

    PERFORM 1450-EDIT-CURRENT-BALANCE:
        Same pattern as credit limit using TEST-NUMVAL-C

    PERFORM 1460-EDIT-CURRENT-CYC-CREDIT:
        Same pattern as credit limit using TEST-NUMVAL-C

    PERFORM 1470-EDIT-CURRENT-CYC-DEBIT:
        Same pattern as credit limit using TEST-NUMVAL-C

    PERFORM 1480-EDIT-OPEN-DATE:
        Validate: CCYYMMDD format
        IF open-date IS NOT NUMERIC
            MOVE 'OPEN DATE MUST BE CCYYMMDD FORMAT' TO WS-MESSAGE
        END-IF
        Validate month (01-12), day (01-31 per month), year > 1900

    PERFORM 1490-EDIT-EXPIRY-DATE:
        Same pattern as open date

    PERFORM 1500-EDIT-REISSUE-DATE:
        Same pattern as open date

    -- Customer Fields
    PERFORM 1510-EDIT-FIRST-NAME:
        Validate: alpha-only, required, max 25 chars
        INSPECT first-name TALLYING non-alpha-count
            FOR ALL CHARACTERS THAT ARE NOT ALPHABETIC AND NOT SPACES
        IF non-alpha-count > 0
            MOVE 'FIRST NAME MUST CONTAIN ONLY ALPHABETIC CHARACTERS' TO WS-MESSAGE
        END-IF
        IF first-name = SPACES
            MOVE 'FIRST NAME IS REQUIRED' TO WS-MESSAGE
        END-IF

    PERFORM 1520-EDIT-MIDDLE-NAME:
        Validate: alpha-only, OPTIONAL, max 25 chars
        IF middle-name NOT = SPACES
            INSPECT middle-name (same alpha-only check)
        END-IF

    PERFORM 1530-EDIT-LAST-NAME:
        Validate: alpha-only, required, max 25 chars
        Same pattern as first name

    PERFORM 1540-EDIT-ADDRESS-LINE-1:
        Validate: mandatory presence check (not blank), max 50 chars
        IF address-line-1 = SPACES
            MOVE 'ADDRESS LINE 1 IS REQUIRED' TO WS-MESSAGE
        END-IF

    PERFORM 1550-EDIT-CITY:
        Validate: alpha-only, required, max 50 chars

    PERFORM 1560-EDIT-STATE:
        Validate: alpha-only, required, 2 chars
        Validate: lookup against state code table (50 US states + territories)
        IF state-code NOT IN valid-state-table
            MOVE 'INVALID STATE CODE' TO WS-MESSAGE
        END-IF

    PERFORM 1570-EDIT-ZIP-CODE:
        Validate: numeric, required, 5 digits
        IF zip-code IS NOT NUMERIC
            MOVE 'ZIP CODE MUST BE 5 DIGIT NUMERIC' TO WS-MESSAGE
        END-IF

    PERFORM 1575-EDIT-STATE-ZIP-CROSS:
        Validate: first 2 digits of ZIP must be valid for the given state code
        EVALUATE state-code
            WHEN 'NY' IF zip-prefix NOT IN (10, 11, 12, 13, 14) -> ERROR
            WHEN 'CA' IF zip-prefix NOT IN (90, 91, 92, 93, 94, 95, 96) -> ERROR
            ... (full state-ZIP mapping)
        END-EVALUATE

    PERFORM 1580-EDIT-COUNTRY:
        Note: Country field is PROTECTED on screen (not user-editable)
        Alpha, 3 chars, but no user validation needed (system-populated)

    PERFORM 1590-EDIT-PHONE-1:
        Validate: optional, (999)999-9999 format
        IF phone-1 NOT = SPACES
            Validate area code: NOT 0xx or 1xx (NANP rules)
            Validate exchange: NOT 0xx or 1xx
            Validate: all digit positions are numeric
        END-IF

    PERFORM 1600-EDIT-PHONE-2:
        Same pattern as phone-1

    PERFORM 1610-EDIT-SSN:
        Validate: required, 9 digits in xxx-xx-xxxx format
        EXTRACT part1 (positions 1-3), part2 (positions 5-6), part3 (positions 8-11)
        IF part1 = '000' OR part1 = '666' OR part1 >= '900'
            MOVE 'INVALID SSN - AREA NUMBER' TO WS-MESSAGE
        END-IF
        IF part2 < '01' OR part2 > '99'
            MOVE 'INVALID SSN - GROUP NUMBER' TO WS-MESSAGE
        END-IF
        IF part3 < '0001' OR part3 > '9999'
            MOVE 'INVALID SSN - SERIAL NUMBER' TO WS-MESSAGE
        END-IF

    PERFORM 1620-EDIT-DATE-OF-BIRTH:
        Validate: CCYYMMDD format + additional DOB-specific validation
        Standard date validation (month/day/year ranges)
        Additional: year must be within reasonable range for a living person

    PERFORM 1630-EDIT-FICO-SCORE:
        Validate: required, 3-digit numeric, range 300-850
        IF fico-score IS NOT NUMERIC
            MOVE 'FICO SCORE MUST BE NUMERIC' TO WS-MESSAGE
        END-IF
        IF fico-score < 300 OR fico-score > 850
            MOVE 'FICO SCORE MUST BE BETWEEN 300 AND 850' TO WS-MESSAGE
        END-IF

    PERFORM 1640-EDIT-EFT-ACCOUNT-ID:
        Validate: required, 10-digit numeric
        IF eft-account-id IS NOT NUMERIC
            MOVE 'EFT ACCOUNT ID MUST BE 10 DIGIT NUMERIC' TO WS-MESSAGE
        END-IF

    PERFORM 1650-EDIT-PRIMARY-CARDHOLDER:
        IF primary-cardholder NOT = 'Y' AND NOT = 'N'
            MOVE 'PRIMARY CARD HOLDER MUST BE Y OR N' TO WS-MESSAGE
            SET input-error = TRUE
        END-IF
```

### Decision Table: Field Validation Summary

| # | Field | Type | Required | Format | Valid Range | Error on Failure |
|---|-------|------|----------|--------|-------------|-----------------|
| 1 | Account ID | Numeric | Yes | PIC 9(11) | Non-zero, 11 digits | 'ACCOUNT ID MUST BE 11 DIGIT NUMERIC' |
| 2 | Active Status | Alpha | Yes | PIC X(1) | 'Y' or 'N' | 'ACTIVE STATUS MUST BE Y OR N' |
| 3 | Credit Limit | Currency | Yes | +ZZZ,ZZZ,ZZZ.99 | S9(10)V99 | 'CREDIT LIMIT MUST BE A VALID CURRENCY AMOUNT' |
| 4 | Cash Credit Limit | Currency | Yes | +ZZZ,ZZZ,ZZZ.99 | S9(10)V99 | 'CASH CREDIT LIMIT MUST BE A VALID CURRENCY AMOUNT' |
| 5 | Current Balance | Currency | Yes | +ZZZ,ZZZ,ZZZ.99 | S9(10)V99 | 'CURRENT BALANCE MUST BE A VALID CURRENCY AMOUNT' |
| 6 | Current Cycle Credit | Currency | Yes | +ZZZ,ZZZ,ZZZ.99 | S9(10)V99 | 'CURRENT CYCLE CREDIT MUST BE A VALID CURRENCY AMOUNT' |
| 7 | Current Cycle Debit | Currency | Yes | +ZZZ,ZZZ,ZZZ.99 | S9(10)V99 | 'CURRENT CYCLE DEBIT MUST BE A VALID CURRENCY AMOUNT' |
| 8 | Open Date | Date | Yes | CCYYMMDD | Valid calendar date | 'OPEN DATE MUST BE CCYYMMDD FORMAT' |
| 9 | Expiry Date | Date | Yes | CCYYMMDD | Valid calendar date | 'EXPIRY DATE MUST BE CCYYMMDD FORMAT' |
| 10 | Reissue Date | Date | Yes | CCYYMMDD | Valid calendar date | 'REISSUE DATE MUST BE CCYYMMDD FORMAT' |
| 11 | First Name | Alpha | Yes | PIC X(25) | Alphabetic + spaces only | 'FIRST NAME MUST CONTAIN ONLY ALPHABETIC CHARACTERS' |
| 12 | Middle Name | Alpha | No | PIC X(25) | Alphabetic + spaces only (if provided) | 'MIDDLE NAME MUST CONTAIN ONLY ALPHABETIC CHARACTERS' |
| 13 | Last Name | Alpha | Yes | PIC X(25) | Alphabetic + spaces only | 'LAST NAME MUST CONTAIN ONLY ALPHABETIC CHARACTERS' |
| 14 | Address Line 1 | Alphanumeric | Yes | PIC X(50) | Non-blank | 'ADDRESS LINE 1 IS REQUIRED' |
| 15 | City | Alpha | Yes | PIC X(50) | Alphabetic + spaces only | 'CITY MUST CONTAIN ONLY ALPHABETIC CHARACTERS' |
| 16 | State | Alpha | Yes | PIC X(2) | Valid US state code | 'INVALID STATE CODE' |
| 17 | ZIP Code | Numeric | Yes | PIC 9(5) | 5 digits | 'ZIP CODE MUST BE 5 DIGIT NUMERIC' |
| 18 | Country | Alpha | N/A | PIC X(3) | Protected field (not user-editable) | N/A |
| 19 | Phone 1 | Formatted | No | (999)999-9999 | NANP: area 200-999, exchange 200-999 | 'PHONE NUMBER FORMAT MUST BE (999)999-9999' |
| 20 | Phone 2 | Formatted | No | (999)999-9999 | Same as Phone 1 | 'PHONE NUMBER FORMAT MUST BE (999)999-9999' |
| 21 | SSN | Formatted | Yes | xxx-xx-xxxx | Area not 000/666/900-999; group 01-99; serial 0001-9999 | 'INVALID SSN' |
| 22 | Date of Birth | Date | Yes | CCYYMMDD | Valid date + reasonable age range | 'DATE OF BIRTH MUST BE CCYYMMDD FORMAT' |
| 23 | FICO Score | Numeric | Yes | PIC 9(3) | 300-850 | 'FICO SCORE MUST BE BETWEEN 300 AND 850' |
| 24 | EFT Account ID | Numeric | Yes | PIC 9(10) | 10 digits | 'EFT ACCOUNT ID MUST BE 10 DIGIT NUMERIC' |
| 25 | Primary Cardholder | Alpha | Yes | PIC X(1) | 'Y' or 'N' | 'PRIMARY CARD HOLDER MUST BE Y OR N' |

### Decision Table: SSN Validation

| SSN Part | Position | Valid Range | Invalid Examples | Rule |
|----------|----------|-------------|-----------------|------|
| Area (part 1) | Digits 1-3 | 001-665, 667-899 | 000, 666, 900-999 | SSA area number restrictions |
| Group (part 2) | Digits 4-5 | 01-99 | 00 | SSA group number must be non-zero |
| Serial (part 3) | Digits 6-9 | 0001-9999 | 0000 | SSA serial number must be non-zero |

### Decision Table: US Phone Number (NANP) Validation

| Component | Position | Valid Range | Invalid Examples | Rule |
|-----------|----------|-------------|-----------------|------|
| Area Code | Digits 1-3 | 200-999 | 000-199 | NANP: first digit must be 2-9 |
| Exchange | Digits 4-6 | 200-999 | 000-199 | NANP: first digit must be 2-9 |
| Subscriber | Digits 7-10 | 0000-9999 | N/A | Any 4-digit combination valid |

### Decision Table: State-ZIP Cross-Validation (excerpt)

| State Code | Valid ZIP Prefixes (first 2 digits) | Example Valid ZIP | Example Invalid ZIP |
|-----------|--------------------------------------|-------------------|---------------------|
| NY | 10, 11, 12, 13, 14 | 10001, 11201 | 90210 |
| CA | 90, 91, 92, 93, 94, 95, 96 | 90210, 94102 | 10001 |
| TX | 73, 75, 76, 77, 78, 79, 88 | 75001, 77001 | 10001 |
| FL | 32, 33, 34 | 33101, 34201 | 10001 |
| IL | 60, 61, 62 | 60601, 62701 | 90210 |

*(Full state-ZIP prefix mapping spans all 50 states plus territories in the COBOL source at lines 2536-2560)*

## Source COBOL Reference

**Program:** `COACTUPC.cbl`
**Lines:** 1429-1676 (main validation orchestration), 1783-2560 (individual edit routines)

**Account ID edit (lines 1783-1822):**

```cobol
001783 9000-EDIT-ACCOUNT.
001784     IF ACCT-ID-INPUT = SPACES OR LOW-VALUES
001785         MOVE 'ACCOUNT ID MUST BE 11 DIGIT NUMERIC'
001786             TO WS-MESSAGE
001787         SET WS-INPUT-ERROR TO TRUE
001788         GO TO 9000-EDIT-ACCOUNT-EXIT
001789     END-IF.
001790
001791     IF ACCT-ID-INPUT IS NOT NUMERIC
001792         MOVE 'ACCOUNT ID MUST BE 11 DIGIT NUMERIC'
001793             TO WS-MESSAGE
001794         SET WS-INPUT-ERROR TO TRUE
001795         GO TO 9000-EDIT-ACCOUNT-EXIT
001796     END-IF.
001797
001798     IF ACCT-ID-INPUT = ZEROS
001799         MOVE 'ACCOUNT ID MUST BE 11 DIGIT NUMERIC'
001800             TO WS-MESSAGE
001801         SET WS-INPUT-ERROR TO TRUE
001802         GO TO 9000-EDIT-ACCOUNT-EXIT
001803     END-IF.
001804
001822 9000-EDIT-ACCOUNT-EXIT.
001823     EXIT.
```

**Yes/No edit (lines 1856-1896):**

```cobol
001856 9100-EDIT-YES-NO.
001857     IF WS-EDIT-YES-NO-FIELD NOT = 'Y'
001858        AND WS-EDIT-YES-NO-FIELD NOT = 'N'
001859         MOVE WS-EDIT-YES-NO-MSG TO WS-MESSAGE
001860         SET WS-INPUT-ERROR TO TRUE
001861         GO TO 9100-EDIT-YES-NO-EXIT
001862     END-IF.
001863
001896 9100-EDIT-YES-NO-EXIT.
001897     EXIT.
```

**Alpha required edit (lines 1898-1953):**

```cobol
001898 9200-EDIT-ALPHA-REQD.
001899     IF WS-EDIT-ALPHA-FIELD = SPACES
001900         MOVE WS-EDIT-ALPHA-MSG TO WS-MESSAGE
001901         SET WS-INPUT-ERROR TO TRUE
001902         GO TO 9200-EDIT-ALPHA-REQD-EXIT
001903     END-IF.
001904
001905     INSPECT WS-EDIT-ALPHA-FIELD
001906         TALLYING WS-EDIT-ALPHA-NON-ALPHA
001907         FOR ALL CHARACTERS THAT ARE NOT ALPHABETIC
001908             AND NOT EQUAL TO SPACES
001909     IF WS-EDIT-ALPHA-NON-ALPHA > 0
001910         MOVE WS-EDIT-ALPHA-MSG TO WS-MESSAGE
001911         SET WS-INPUT-ERROR TO TRUE
001912         GO TO 9200-EDIT-ALPHA-REQD-EXIT
001913     END-IF.
001914
001953 9200-EDIT-ALPHA-REQD-EXIT.
001954     EXIT.
```

**Signed currency edit using TEST-NUMVAL-C (lines 2180-2223):**

```cobol
002180 9500-EDIT-SIGNED-CURRENCY.
002181     COMPUTE WS-TEST-RESULT =
002182         FUNCTION TEST-NUMVAL-C(WS-EDIT-CURRENCY-FIELD)
002183     IF WS-TEST-RESULT > 0
002184         MOVE WS-EDIT-CURRENCY-MSG TO WS-MESSAGE
002185         SET WS-INPUT-ERROR TO TRUE
002186         GO TO 9500-EDIT-SIGNED-CURRENCY-EXIT
002187     ELSE
002188         COMPUTE WS-EDIT-CURRENCY-VALUE =
002189             FUNCTION NUMVAL-C(WS-EDIT-CURRENCY-FIELD)
002190     END-IF.
002191
002223 9500-EDIT-SIGNED-CURRENCY-EXIT.
002224     EXIT.
```

**US phone number edit (lines 2225-2429):**

```cobol
002225 9600-EDIT-US-PHONE.
002226     IF WS-EDIT-PHONE-FIELD = SPACES
002227         GO TO 9600-EDIT-US-PHONE-EXIT
002228     END-IF.
002229
002230     IF WS-EDIT-PHONE-FIELD(1:1) NOT = '('
002231         MOVE 'PHONE NUMBER FORMAT MUST BE (999)999-9999'
002232             TO WS-MESSAGE
002233         SET WS-INPUT-ERROR TO TRUE
002234         GO TO 9600-EDIT-US-PHONE-EXIT
002235     END-IF.
002236
002240     MOVE WS-EDIT-PHONE-FIELD(2:3) TO WS-PHONE-AREA-CODE.
002241     IF WS-PHONE-AREA-CODE IS NOT NUMERIC
002242         MOVE 'PHONE AREA CODE MUST BE NUMERIC'
002243             TO WS-MESSAGE
002244         SET WS-INPUT-ERROR TO TRUE
002245         GO TO 9600-EDIT-US-PHONE-EXIT
002246     END-IF.
002247
002250     IF WS-PHONE-AREA-CODE(1:1) = '0'
002251        OR WS-PHONE-AREA-CODE(1:1) = '1'
002252         MOVE 'PHONE AREA CODE CANNOT START WITH 0 OR 1'
002253             TO WS-MESSAGE
002254         SET WS-INPUT-ERROR TO TRUE
002255         GO TO 9600-EDIT-US-PHONE-EXIT
002256     END-IF.
002257
002429 9600-EDIT-US-PHONE-EXIT.
002430     EXIT.
```

**SSN edit (lines 2431-2491):**

```cobol
002431 9700-EDIT-SSN.
002432     MOVE WS-SSN-INPUT(1:3) TO WS-SSN-PART1.
002433     MOVE WS-SSN-INPUT(5:2) TO WS-SSN-PART2.
002434     MOVE WS-SSN-INPUT(8:4) TO WS-SSN-PART3.
002435
002436     IF WS-SSN-PART1 IS NOT NUMERIC
002437        OR WS-SSN-PART2 IS NOT NUMERIC
002438        OR WS-SSN-PART3 IS NOT NUMERIC
002439         MOVE 'SSN MUST BE 9 DIGITS IN XXX-XX-XXXX FORMAT'
002440             TO WS-MESSAGE
002441         SET WS-INPUT-ERROR TO TRUE
002442         GO TO 9700-EDIT-SSN-EXIT
002443     END-IF.
002444
002445     IF WS-SSN-PART1 = '000'
002446        OR WS-SSN-PART1 = '666'
002447         MOVE 'INVALID SSN - AREA NUMBER'
002448             TO WS-MESSAGE
002449         SET WS-INPUT-ERROR TO TRUE
002450         GO TO 9700-EDIT-SSN-EXIT
002451     END-IF.
002452
002453     IF WS-SSN-PART1 >= '900'
002454         MOVE 'INVALID SSN - AREA NUMBER'
002455             TO WS-MESSAGE
002456         SET WS-INPUT-ERROR TO TRUE
002457         GO TO 9700-EDIT-SSN-EXIT
002458     END-IF.
002459
002460     IF WS-SSN-PART2 = '00'
002461         MOVE 'INVALID SSN - GROUP NUMBER'
002462             TO WS-MESSAGE
002463         SET WS-INPUT-ERROR TO TRUE
002464         GO TO 9700-EDIT-SSN-EXIT
002465     END-IF.
002466
002467     IF WS-SSN-PART3 = '0000'
002468         MOVE 'INVALID SSN - SERIAL NUMBER'
002469             TO WS-MESSAGE
002470         SET WS-INPUT-ERROR TO TRUE
002471         GO TO 9700-EDIT-SSN-EXIT
002472     END-IF.
002473
002491 9700-EDIT-SSN-EXIT.
002492     EXIT.
```

**State code edit (lines 2493-2512):**

```cobol
002493 9800-EDIT-STATE-CODE.
002494     EVALUATE WS-STATE-CODE
002495         WHEN 'AL' WHEN 'AK' WHEN 'AZ' WHEN 'AR' WHEN 'CA'
002496         WHEN 'CO' WHEN 'CT' WHEN 'DE' WHEN 'FL' WHEN 'GA'
002497         WHEN 'HI' WHEN 'ID' WHEN 'IL' WHEN 'IN' WHEN 'IA'
002498         WHEN 'KS' WHEN 'KY' WHEN 'LA' WHEN 'ME' WHEN 'MD'
002499         WHEN 'MA' WHEN 'MI' WHEN 'MN' WHEN 'MS' WHEN 'MO'
002500         WHEN 'MT' WHEN 'NE' WHEN 'NV' WHEN 'NH' WHEN 'NJ'
002501         WHEN 'NM' WHEN 'NY' WHEN 'NC' WHEN 'ND' WHEN 'OH'
002502         WHEN 'OK' WHEN 'OR' WHEN 'PA' WHEN 'RI' WHEN 'SC'
002503         WHEN 'SD' WHEN 'TN' WHEN 'TX' WHEN 'UT' WHEN 'VT'
002504         WHEN 'VA' WHEN 'WA' WHEN 'WV' WHEN 'WI' WHEN 'WY'
002505             CONTINUE
002506         WHEN OTHER
002507             MOVE 'INVALID STATE CODE' TO WS-MESSAGE
002508             SET WS-INPUT-ERROR TO TRUE
002509     END-EVALUATE.
002510
002512 9800-EDIT-STATE-CODE-EXIT.
002513     EXIT.
```

**FICO score range edit (lines 2514-2533):**

```cobol
002514 9850-EDIT-FICO-SCORE.
002515     IF WS-FICO-SCORE IS NOT NUMERIC
002516         MOVE 'FICO SCORE MUST BE NUMERIC'
002517             TO WS-MESSAGE
002518         SET WS-INPUT-ERROR TO TRUE
002519         GO TO 9850-EDIT-FICO-SCORE-EXIT
002520     END-IF.
002521
002522     IF WS-FICO-SCORE < 300
002523        OR WS-FICO-SCORE > 850
002524         MOVE 'FICO SCORE MUST BE BETWEEN 300 AND 850'
002525             TO WS-MESSAGE
002526         SET WS-INPUT-ERROR TO TRUE
002527         GO TO 9850-EDIT-FICO-SCORE-EXIT
002528     END-IF.
002529
002533 9850-EDIT-FICO-SCORE-EXIT.
002534     EXIT.
```

**State-ZIP cross-validation (lines 2536-2560):**

```cobol
002536 9900-EDIT-STATE-ZIP-CROSS.
002537     MOVE WS-ZIP-CODE(1:2) TO WS-ZIP-PREFIX.
002538
002539     EVALUATE WS-STATE-CODE
002540         WHEN 'NY'
002541             IF WS-ZIP-PREFIX NOT = '10'
002542                AND WS-ZIP-PREFIX NOT = '11'
002543                AND WS-ZIP-PREFIX NOT = '12'
002544                AND WS-ZIP-PREFIX NOT = '13'
002545                AND WS-ZIP-PREFIX NOT = '14'
002546                 MOVE 'ZIP CODE DOES NOT MATCH STATE'
002547                     TO WS-MESSAGE
002548                 SET WS-INPUT-ERROR TO TRUE
002549             END-IF
002550         WHEN 'CA'
002551             IF WS-ZIP-PREFIX NOT = '90'
002552                AND WS-ZIP-PREFIX NOT = '91'
002553                ...
002554             END-IF
002555         ...
002556     END-EVALUATE.
002557
002560 9900-EDIT-STATE-ZIP-CROSS-EXIT.
002561     EXIT.
```

**Currency display format (line 371):**

```cobol
000371     05  WS-CURRENCY-DISPLAY     PIC +ZZZ,ZZZ,ZZZ.99.
```

## Acceptance Criteria

### Scenario 1: Valid account update with all fields passing validation

```gherkin
GIVEN the user enters valid values for all 25 fields:
  | Field              | Value             |
  | Account ID         | 12345678901       |
  | Active Status      | Y                 |
  | Credit Limit       | +5,000.00         |
  | Cash Credit Limit  | +1,000.00         |
  | Current Balance    | +2,345.67         |
  | Open Date          | 20200115          |
  | First Name         | JOHN              |
  | Last Name          | DOE               |
  | SSN                | 123-45-6789       |
  | FICO Score         | 750               |
  | State              | NY                |
  | ZIP Code           | 10001             |
WHEN the validation is performed
THEN all fields pass validation
  AND the account and customer records are updated in ACCTDAT and CUSTDAT
```

### Scenario 2: Account ID validation failure

```gherkin
GIVEN the user enters "ABCDEFGHIJK" as the account ID
WHEN the validation is performed
THEN the error message "ACCOUNT ID MUST BE 11 DIGIT NUMERIC" is displayed
  AND no update is performed
```

### Scenario 3: Active status must be Y or N

```gherkin
GIVEN the user enters "X" as the active status
WHEN the validation is performed
THEN the error message "ACTIVE STATUS MUST BE Y OR N" is displayed
  AND no update is performed
```

### Scenario 4: Currency field validation with TEST-NUMVAL-C

```gherkin
GIVEN the user enters "ABC" as the credit limit
WHEN the credit limit is validated using TEST-NUMVAL-C
THEN the error message "CREDIT LIMIT MUST BE A VALID CURRENCY AMOUNT" is displayed
  AND no update is performed
```

### Scenario 5: Valid currency amount is parsed correctly

```gherkin
GIVEN the user enters "+5,000.50" as the credit limit
WHEN the credit limit is validated using TEST-NUMVAL-C
THEN TEST-NUMVAL-C returns 0 (valid)
  AND NUMVAL-C converts it to the numeric value 5000.50
  AND the value is stored as PIC S9(10)V99 = +0000005000.50
```

### Scenario 6: Date validation rejects invalid dates

```gherkin
GIVEN the user enters "20241345" as the open date
WHEN the date validation is performed
THEN the error message "OPEN DATE MUST BE CCYYMMDD FORMAT" is displayed
  AND month 13 is recognized as invalid (valid range: 01-12)
```

### Scenario 7: Alpha-only field rejects numeric characters

```gherkin
GIVEN the user enters "JOHN123" as the first name
WHEN the first name validation is performed
THEN the error message "FIRST NAME MUST CONTAIN ONLY ALPHABETIC CHARACTERS" is displayed
  AND no update is performed
```

### Scenario 8: Middle name is optional

```gherkin
GIVEN the user leaves the middle name field blank (spaces)
WHEN the validation is performed
THEN the middle name passes validation without error
  AND remaining fields continue to be validated
```

### Scenario 9: SSN area number restrictions

```gherkin
GIVEN the user enters SSN "000-12-3456"
WHEN the SSN validation is performed
THEN the error message "INVALID SSN - AREA NUMBER" is displayed
  AND area number 000 is recognized as invalid per SSA rules
```

### Scenario 10: SSN area number 666 rejected

```gherkin
GIVEN the user enters SSN "666-12-3456"
WHEN the SSN validation is performed
THEN the error message "INVALID SSN - AREA NUMBER" is displayed
  AND area number 666 is specifically excluded per SSA rules
```

### Scenario 11: SSN area number 900-999 rejected

```gherkin
GIVEN the user enters SSN "901-12-3456"
WHEN the SSN validation is performed
THEN the error message "INVALID SSN - AREA NUMBER" is displayed
  AND area numbers 900-999 (ITIN range) are excluded per SSA rules
```

### Scenario 12: SSN group number must be non-zero

```gherkin
GIVEN the user enters SSN "123-00-4567"
WHEN the SSN validation is performed
THEN the error message "INVALID SSN - GROUP NUMBER" is displayed
  AND group number 00 is recognized as invalid
```

### Scenario 13: SSN serial number must be non-zero

```gherkin
GIVEN the user enters SSN "123-45-0000"
WHEN the SSN validation is performed
THEN the error message "INVALID SSN - SERIAL NUMBER" is displayed
  AND serial number 0000 is recognized as invalid
```

### Scenario 14: FICO score below minimum

```gherkin
GIVEN the user enters "250" as the FICO score
WHEN the FICO validation is performed
THEN the error message "FICO SCORE MUST BE BETWEEN 300 AND 850" is displayed
```

### Scenario 15: FICO score above maximum

```gherkin
GIVEN the user enters "900" as the FICO score
WHEN the FICO validation is performed
THEN the error message "FICO SCORE MUST BE BETWEEN 300 AND 850" is displayed
```

### Scenario 16: FICO score boundary values accepted

```gherkin
GIVEN the user enters "300" as the FICO score
WHEN the FICO validation is performed
THEN the validation passes without error

GIVEN the user enters "850" as the FICO score
WHEN the FICO validation is performed
THEN the validation passes without error
```

### Scenario 17: State code lookup validation

```gherkin
GIVEN the user enters "XX" as the state code
WHEN the state code is validated against the lookup table
THEN the error message "INVALID STATE CODE" is displayed
  AND only the 50 US state codes are accepted
```

### Scenario 18: State-ZIP cross-validation failure

```gherkin
GIVEN the user enters state "NY" and ZIP code "90210"
WHEN the state-ZIP cross-validation is performed
THEN the error message "ZIP CODE DOES NOT MATCH STATE" is displayed
  AND ZIP prefix "90" is not valid for state "NY" (valid: 10, 11, 12, 13, 14)
```

### Scenario 19: State-ZIP cross-validation success

```gherkin
GIVEN the user enters state "NY" and ZIP code "10001"
WHEN the state-ZIP cross-validation is performed
THEN the validation passes without error
  AND ZIP prefix "10" is valid for state "NY"
```

### Scenario 20: Phone number NANP validation

```gherkin
GIVEN the user enters "(012)345-6789" as phone number 1
WHEN the phone number validation is performed
THEN an error is displayed because area code starts with '0'
  AND NANP rules require area codes to start with digits 2-9
```

### Scenario 21: Phone number is optional

```gherkin
GIVEN the user leaves phone number 1 blank (spaces)
WHEN the phone number validation is performed
THEN the phone number passes validation without error
  AND the blank value is accepted as "no phone number provided"
```

### Scenario 22: Country field is protected

```gherkin
GIVEN the account update screen is displayed
WHEN the user attempts to modify the country field
THEN the field is not editable (BMS PROTECTED attribute)
  AND the country value is system-populated and cannot be changed by the user
```

### Scenario 23: First validation error halts processing

```gherkin
GIVEN the user enters invalid values for both account ID and credit limit
WHEN the validation is performed
THEN only the account ID error message is displayed (first field in validation order)
  AND the credit limit validation is not reached
  AND no update is performed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 S3 | Credit institutions must have adequate systems for managing operational risk, including input validation and data integrity controls | The 25-field validation suite prevents corrupt or malformed data from entering the account and customer master files. Each field has explicit format, range, and cross-field checks that enforce data quality at the point of entry |
| PSD2 | Art. 97 | Strong customer authentication for modifying payment account information | The account update transaction requires CICS terminal authentication. The migrated system must enforce SCA before allowing any account or customer data modification. The validation layer acts as a secondary defense against unauthorized data changes |
| GDPR | Art. 5(1)(d) | Accuracy -- personal data must be accurate and kept up to date | The validation rules for customer fields (name, address, SSN, DOB) enforce data accuracy at input time. Alpha-only checks for names, state code lookups, and state-ZIP cross-validation help ensure address data integrity. The migrated system must maintain equivalent validation rigor |
| AML/KYC | 4th Anti-Money Laundering Directive | Customer due diligence requires accurate identification data | SSN validation (area/group/serial number restrictions) ensures the SSN conforms to SSA rules, supporting identity verification. Date of birth validation supports age verification. FICO score validation ensures credit risk data integrity for risk-based customer assessment |

## Edge Cases

1. **TEST-NUMVAL-C currency parsing**: The COBOL FUNCTION TEST-NUMVAL-C intrinsic accepts multiple currency formats: "+1,234.56", "-1234.56", "1234.56CR", "1234.56DB", "$1,234.56". The migrated .NET system must replicate this flexible parsing or define a strict subset of accepted formats and document the behavioral difference. Using `decimal.TryParse` with `NumberStyles.Currency` in .NET may not match all COBOL-accepted formats exactly.

2. **First-error-only reporting**: The COBOL validation uses a fail-fast pattern where the first field error halts all subsequent validation (via GO TO exit paragraphs). The migrated system should consider collecting all validation errors in a single pass and returning them together, which is a common REST API pattern. However, this changes the user experience and must be documented as a deliberate behavioral change from the COBOL original.

3. **State code table limited to 50 US states**: The COBOL EVALUATE statement lists exactly the 50 US state codes. It does not include US territories (PR, GU, VI, AS, MP) or military codes (AA, AE, AP). If NordKredit has customers with addresses in US territories, these would be rejected by the current validation. The migrated system should confirm whether territory codes need to be added.

4. **SSN format assumes US Social Security**: The SSN validation follows US Social Security Administration rules. For the NordKredit Swedish context, the equivalent identifier would be the Swedish personnummer (YYYYMMDD-NNNN). The migration must determine whether to replace SSN validation with personnummer validation or maintain both for international customers.

5. **FICO score 300-850 range**: The FICO score range 300-850 is specific to the US credit scoring system. Sweden uses different credit scoring providers (UC, Creditsafe) with different score ranges. The migrated system must determine whether to maintain the FICO range, add support for Swedish credit scores, or make the range configurable.

6. **Alpha-only validation and Swedish characters**: The COBOL ALPHABETIC check recognizes only A-Z and spaces (uppercase and lowercase in EBCDIC). Swedish names containing characters with diacritics (such as A-ring, A-umlaut, O-umlaut) would fail the ALPHABETIC check. The migrated system must support the full Unicode character set for Swedish names, which is a functional enhancement over the COBOL original.

7. **Date validation and leap years**: The COBOL date validation for CCYYMMDD must correctly handle February 29 in leap years. The migrated .NET system can use `DateOnly.TryParseExact` which handles leap year logic automatically, but the COBOL implementation should be verified to ensure it also correctly handles this case (some COBOL date validation routines have leap year bugs).

8. **Concurrent update conflicts**: The COACTUPC program reads the account record, displays it for editing, and then writes the update. Between the read and the write, another user could modify the same record. The COBOL program does not implement optimistic concurrency control (no version/timestamp check). The migrated system should add optimistic concurrency (ETag or row version) to prevent lost updates, which is a functional enhancement.

9. **Country field protection bypass**: Although the country field is set to PROTECTED (BMS attribute), a sophisticated user could potentially bypass BMS field protection through a modified terminal emulator. The COBOL program does not server-side validate the country field since it assumes BMS protection is sufficient. The migrated REST API must validate the country field server-side since client-side protection is trivially bypassed in web applications.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Critical questions for the retiring COBOL developers: (1) Does the TEST-NUMVAL-C intrinsic accept currency symbols (e.g., "$" or "kr") in the input, or only pure numeric formats with optional sign and commas? (2) Is the state-ZIP cross-validation table at lines 2536-2560 complete and up-to-date, or are there known gaps? (3) Should the migrated system replace US SSN validation with Swedish personnummer validation? (4) Are there any undocumented fields that are validated outside the main 1400-VALIDATE-ALL-FIELDS paragraph? (5) Has the FICO score range (300-850) ever been adjusted, and should it be made configurable for Swedish credit scoring? (6) Does the alpha-only check for names cause problems for customers with non-ASCII characters in their names?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
