---
id: "rept-br-010"
title: "Multi-record export data structure (CVEXPORT copybook)"
domain: "reporting"
cobol_source: "CVEXPORT.cpy:1-104"
requirement_id: "REPT-BR-010"
regulations:
  - "GDPR Art. 5 (data structure documentation)"
  - "FFFS 2014:5 (data integrity)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# REPT-BR-010: Multi-record export data structure (CVEXPORT copybook)

## Summary

The CVEXPORT copybook defines the 500-byte fixed-length export record layout used by both the CBEXPORT (REPT-BR-008) and CBIMPORT (REPT-BR-009) programs. The record consists of a common header section and a 460-byte type-specific payload area. The header contains the record type indicator (1 byte), a 26-character timestamp with a REDEFINES structure breaking it into date (10 chars), separator (1 char), and time (15 chars) components, a COMP sequence number serving as the record key, a 4-character branch ID, and a 5-character region code. The payload area (EXPORT-RECORD-DATA) is defined as PIC X(460) with five REDEFINES overlays corresponding to the five record types: customer, account, transaction, card cross-reference, and card. Numeric fields use storage-optimized formats including COMP (binary) for integer identifiers and COMP-3 (packed decimal) for financial amounts and scores, matching the mainframe's native numeric representations that must be carefully converted during .NET migration.

## Business Logic

### Pseudocode

```
STRUCTURE ExportRecord (500 bytes total):

    // Common Header (40 bytes)
    FIELD RecordType         : CHAR(1)      // 'C','A','X','T','D'
    FIELD Timestamp          : CHAR(26)     // YYYY-MM-DD-HH.MM.SS.NNNNNN
        REDEFINES AS:
            TimestampDate    : CHAR(10)     // YYYY-MM-DD
            TimestampSep     : CHAR(1)      // '-'
            TimestampTime    : CHAR(15)     // HH.MM.SS.NNNNNN
    FIELD SequenceNum        : INTEGER(9) COMP    // Unique key, binary
    FIELD BranchId           : CHAR(4)      // Originating branch
    FIELD RegionCode         : CHAR(5)      // Geographic region

    // Type-Specific Payload (460 bytes)
    FIELD RecordData         : CHAR(460)

    // REDEFINES 1: Customer Record (when RecordType = 'C')
    OVERLAY CustomerData ON RecordData:
        CustId              : INTEGER COMP          // Customer ID
        CustFirstName       : CHAR(25)              // First name
        CustMiddleName      : CHAR(25)              // Middle name
        CustLastName        : CHAR(25)              // Last name
        CustAddrLine1       : CHAR(50)              // Address line 1
        CustAddrLine2       : CHAR(50)              // Address line 2
        CustAddrLine3       : CHAR(50)              // Address line 3
        CustState           : CHAR(2)               // State code
        CustCountry          : CHAR(3)               // Country code
        CustZipCode         : CHAR(10)              // ZIP/postal code
        CustPhone1          : CHAR(15)              // Primary phone
        CustPhone2          : CHAR(15)              // Secondary phone
        CustSSN             : CHAR(9)               // Social Security Number [PII]
        CustGovtId          : CHAR(20)              // Government-issued ID [PII]
        CustDOB             : CHAR(10)              // Date of birth [PII]
        CustEFTAccount      : CHAR(10)              // EFT account ID
        CustPrimaryHolder   : CHAR(1)               // Primary cardholder indicator
        CustFICOScore       : PACKED-DECIMAL COMP-3 // Credit score

    // REDEFINES 2: Account Record (when RecordType = 'A')
    OVERLAY AccountData ON RecordData:
        AcctId              : CHAR(11)              // Account ID
        AcctStatus          : CHAR(1)               // Active status
        AcctBalance         : PACKED-DECIMAL COMP-3 // Current balance
        AcctCreditLimit     : PACKED-DECIMAL COMP-3 // Credit limit
        AcctCashCreditLimit : PACKED-DECIMAL COMP-3 // Cash credit limit
        AcctOpenDate        : CHAR(10)              // Open date
        AcctExpiryDate      : CHAR(10)              // Expiration date
        AcctReissueDate     : CHAR(10)              // Reissue date
        AcctCycleCredit     : INTEGER COMP          // Current cycle credit
        AcctCycleDebit      : INTEGER COMP          // Current cycle debit
        AcctZip             : CHAR(10)              // Account ZIP code
        AcctGroupId         : CHAR(10)              // Group identifier

    // REDEFINES 3: Transaction Record (when RecordType = 'T')
    OVERLAY TransactionData ON RecordData:
        TranId              : CHAR(16)              // Transaction ID
        TranType            : CHAR(2)               // Type code
        TranCategory        : CHAR(4)               // Category code
        TranSource          : CHAR(10)              // Source identifier
        TranDesc            : CHAR(100)             // Description
        TranAmount          : PACKED-DECIMAL COMP-3 // Amount
        TranMerchantId      : CHAR(9)               // Merchant ID
        TranMerchantName    : CHAR(50)              // Merchant name
        TranMerchantCity    : CHAR(30)              // Merchant city
        TranMerchantZip     : CHAR(10)              // Merchant ZIP
        TranCardNum         : CHAR(16)              // Card number [PII]
        TranOrigTimestamp   : CHAR(26)              // Origination timestamp
        TranProcTimestamp   : CHAR(26)              // Processing timestamp

    // REDEFINES 4: Card Cross-Reference (when RecordType = 'X')
    OVERLAY CardXRefData ON RecordData:
        XRefCardNum         : CHAR(16)              // Card number
        XRefCustId          : INTEGER COMP          // Customer ID
        XRefAcctId          : INTEGER COMP          // Account ID

    // REDEFINES 5: Card Record (when RecordType = 'D')
    OVERLAY CardData ON RecordData:
        CardNum             : CHAR(16)              // Card number [PII]
        CardAcctId          : INTEGER COMP          // Account ID
        CardCVV             : INTEGER COMP          // CVV code [PII/PCI]
        CardEmbossedName    : CHAR(50)              // Embossed name
        CardExpiryDate      : CHAR(10)              // Expiration date
        CardActiveStatus    : CHAR(1)               // Active status indicator
```

### Decision Table

| Record Type | Overlay Name | COMP Fields | COMP-3 Fields | PII Fields | Approximate Payload Usage |
|-------------|-------------|-------------|---------------|------------|--------------------------|
| 'C' (Customer) | CustomerData | CustId | CustFICOScore | SSN, GovtId, DOB, Phone, Address | High (multiple address and identity fields) |
| 'A' (Account) | AccountData | AcctCycleCredit, AcctCycleDebit | AcctBalance, AcctCreditLimit, AcctCashCreditLimit | None | Medium (financial amounts) |
| 'T' (Transaction) | TransactionData | None | TranAmount | CardNum | High (description and merchant fields) |
| 'X' (Card XRef) | CardXRefData | XRefCustId, XRefAcctId | None | CardNum | Low (3 fields only) |
| 'D' (Card) | CardData | CardAcctId, CardCVV | None | CardNum, CVV, EmbossedName | Low-Medium |

## Source COBOL Reference

**Program:** `CVEXPORT.cpy`
**Lines:** 1-104 (Complete copybook definition)

Common header fields (lines 9-19):
```cobol
000009 01  EXPORT-RECORD.
000010     05 EXPORT-REC-TYPE          PIC X(1).
000011     05 EXPORT-TIMESTAMP         PIC X(26).
000012     05 EXPORT-TS-REDEFINES REDEFINES EXPORT-TIMESTAMP.
000013        10 EXPORT-TS-DATE        PIC X(10).
000014        10 EXPORT-TS-SEP         PIC X(1).
000015        10 EXPORT-TS-TIME        PIC X(15).
000016     05 EXPORT-SEQUENCE-NUM      PIC 9(9) COMP.
000017     05 EXPORT-BRANCH-ID         PIC X(4).
000018     05 EXPORT-REGION-CODE       PIC X(5).
000019     05 EXPORT-RECORD-DATA       PIC X(460).
```

Customer overlay (lines 24-42):
```cobol
000024     05 EXP-CUSTOMER-DATA REDEFINES EXPORT-RECORD-DATA.
000025        10 EXP-CUST-ID           PIC 9(9) COMP.
000026        10 EXP-CUST-FIRST-NAME   PIC X(25).
000027        10 EXP-CUST-MIDDLE-NAME  PIC X(25).
000028        10 EXP-CUST-LAST-NAME    PIC X(25).
000029        10 EXP-CUST-ADDR-LINE-1  PIC X(50).
000030        10 EXP-CUST-ADDR-LINE-2  PIC X(50).
000031        10 EXP-CUST-ADDR-LINE-3  PIC X(50).
000032        10 EXP-CUST-ADDR-STATE   PIC X(2).
000033        10 EXP-CUST-ADDR-COUNTRY PIC X(3).
000034        10 EXP-CUST-ADDR-ZIP     PIC X(10).
000035        10 EXP-CUST-PHONE-1      PIC X(15).
000036        10 EXP-CUST-PHONE-2      PIC X(15).
000037        10 EXP-CUST-SSN          PIC X(9).
000038        10 EXP-CUST-GOVT-ID      PIC X(20).
000039        10 EXP-CUST-DOB          PIC X(10).
000040        10 EXP-CUST-EFT-ACCT     PIC X(10).
000041        10 EXP-CUST-PRI-HOLDER   PIC X(1).
000042        10 EXP-CUST-FICO         PIC 9(3) COMP-3.
```

Account overlay (lines 47-60):
```cobol
000047     05 EXP-ACCOUNT-DATA REDEFINES EXPORT-RECORD-DATA.
000048        10 EXP-ACCT-ID           PIC X(11).
000049        10 EXP-ACCT-STATUS       PIC X(1).
000050        10 EXP-ACCT-BALANCE      PIC S9(13)V99 COMP-3.
000051        10 EXP-ACCT-CREDIT-LIMIT PIC S9(13)V99 COMP-3.
000052        10 EXP-ACCT-CASH-LIMIT   PIC S9(13)V99 COMP-3.
000053        10 EXP-ACCT-OPEN-DATE    PIC X(10).
000054        10 EXP-ACCT-EXPIRY-DATE  PIC X(10).
000055        10 EXP-ACCT-REISSUE-DATE PIC X(10).
000056        10 EXP-ACCT-CYC-CREDIT   PIC S9(9) COMP.
000057        10 EXP-ACCT-CYC-DEBIT    PIC S9(9) COMP.
000058        10 EXP-ACCT-ZIP          PIC X(10).
000059        10 EXP-ACCT-GROUP        PIC X(10).
000060
```

Transaction overlay (lines 65-79):
```cobol
000065     05 EXP-TRANSACTION-DATA REDEFINES EXPORT-RECORD-DATA.
000066        10 EXP-TRAN-ID           PIC X(16).
000067        10 EXP-TRAN-TYPE         PIC X(2).
000068        10 EXP-TRAN-CATEGORY     PIC X(4).
000069        10 EXP-TRAN-SOURCE       PIC X(10).
000070        10 EXP-TRAN-DESC         PIC X(100).
000071        10 EXP-TRAN-AMOUNT       PIC S9(11)V99 COMP-3.
000072        10 EXP-TRAN-MERCHANT-ID  PIC X(9).
000073        10 EXP-TRAN-MERCHANT-NAME PIC X(50).
000074        10 EXP-TRAN-MERCHANT-CITY PIC X(30).
000075        10 EXP-TRAN-MERCHANT-ZIP PIC X(10).
000076        10 EXP-TRAN-CARD-NUM     PIC X(16).
000077        10 EXP-TRAN-ORIG-TS      PIC X(26).
000078        10 EXP-TRAN-PROC-TS      PIC X(26).
000079
```

Card cross-reference overlay (lines 84-88):
```cobol
000084     05 EXP-CARD-XREF-DATA REDEFINES EXPORT-RECORD-DATA.
000085        10 EXP-XREF-CARD-NUM     PIC X(16).
000086        10 EXP-XREF-CUST-ID      PIC 9(9) COMP.
000087        10 EXP-XREF-ACCT-ID      PIC 9(9) COMP.
000088
```

Card overlay (lines 93-100):
```cobol
000093     05 EXP-CARD-DATA REDEFINES EXPORT-RECORD-DATA.
000094        10 EXP-CARD-NUM          PIC X(16).
000095        10 EXP-CARD-ACCT-ID      PIC 9(9) COMP.
000096        10 EXP-CARD-CVV          PIC 9(3) COMP.
000097        10 EXP-CARD-EMBOSSED-NAME PIC X(50).
000098        10 EXP-CARD-EXPIRY       PIC X(10).
000099        10 EXP-CARD-ACTIVE       PIC X(1).
000100
```

## Acceptance Criteria

### Scenario 1: Export record is exactly 500 bytes

```gherkin
Given the CVEXPORT copybook defines the export record structure
When a record is created using this layout
Then the total record length is exactly 500 bytes
And the header section occupies the first 40 bytes
And the payload section occupies the remaining 460 bytes
```

### Scenario 2: Record type indicator correctly identifies payload overlay

```gherkin
Given an export record with record type 'C'
When the record is read by the import program
Then the EXPORT-RECORD-DATA is interpreted using the customer overlay
And the customer ID field starts at the beginning of the payload area
And the FICO score field uses COMP-3 packed decimal format
```

### Scenario 3: Timestamp REDEFINES provides date and time components

```gherkin
Given an export record with timestamp "2026-02-15-10.30.45.123456"
When the timestamp REDEFINES is applied
Then the date component contains "2026-02-15"
And the separator contains "-"
And the time component contains "10.30.45.123456"
```

### Scenario 4: COMP fields are correctly converted from binary

```gherkin
Given an export record with a customer ID stored as PIC 9(9) COMP
When the binary value is read in the .NET implementation
Then the value is correctly converted from EBCDIC big-endian binary to a .NET integer
And no precision is lost during conversion
```

### Scenario 5: COMP-3 packed decimal fields preserve precision

```gherkin
Given an account record with balance PIC S9(13)V99 COMP-3
And the balance value is -1234567890123.45
When the packed decimal value is converted in the .NET implementation
Then the value is correctly converted to a .NET decimal type
And the sign is preserved (negative)
And the two implied decimal places are correctly positioned
```

### Scenario 6: Card cross-reference uses minimal payload

```gherkin
Given an export record with record type 'X'
When the card cross-reference overlay is applied
Then only three fields are populated: card number (16 bytes), customer ID (COMP), and account ID (COMP)
And the remaining bytes of the 460-byte payload are unused filler
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5 (data structure documentation) | Processing of personal data must be transparent and documented | The copybook provides a complete, formal specification of every PII field included in the export record, its exact position, size, and encoding. This documentation supports data mapping, impact assessments, and subject access requests |
| FFFS 2014:5 | Data integrity | Financial data structures must ensure integrity during processing and storage | Fixed-length records with precisely defined field positions, COMP/COMP-3 encoding for numeric accuracy, and REDEFINES for type-safe payload interpretation ensure data integrity across export and import operations |

## Edge Cases

1. **COMP byte ordering**: PIC 9(9) COMP on IBM mainframes is stored as big-endian binary (4 bytes for values up to 999,999,999). The .NET implementation (little-endian) must reverse byte order during conversion. Incorrect conversion will produce wildly wrong numeric values.
2. **COMP-3 sign nibble**: COMP-3 packed decimal encodes the sign in the low nibble of the last byte (C=positive, D=negative, F=unsigned). The .NET implementation must correctly interpret all three sign conventions, as different COBOL compilers may use different conventions.
3. **Payload overlay alignment**: The five REDEFINES overlays share the same 460-byte area. Each overlay uses a different amount of the 460 bytes; unused bytes at the end of shorter overlays (e.g., card cross-reference uses only ~24 bytes) contain residual data from INITIALIZE or previous record contents. The .NET implementation must not interpret these trailing bytes.
4. **FICO score COMP-3 range**: PIC 9(3) COMP-3 stores values 000-999 in 2 bytes. FICO scores typically range from 300-850. Values outside this range (e.g., 000 for unscored customers) must be handled as valid but flagged for business review.
5. **Account balance sign handling**: PIC S9(13)V99 COMP-3 supports signed values with 13 integer digits and 2 decimal digits. This accommodates balances up to +/- 9,999,999,999,999.99. The .NET decimal type can represent this range without loss of precision.
6. **Timestamp format consistency**: The timestamp uses a period (.) as the time separator rather than a colon (:), following IBM mainframe conventions. The .NET implementation must parse this non-standard format correctly.
7. **EXPORT-SEQUENCE-NUM as record key**: The sequence number (PIC 9(9) COMP) serves as the VSAM KSDS key for the export file. In .NET, this should map to a primary key or unique index on the equivalent database table or file structure.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The COMP and COMP-3 field conversions are the highest risk area for data corruption during migration. Recommend creating a comprehensive conversion test suite with boundary values for each numeric field: minimum, maximum, zero, negative (for signed fields), and typical business values. Special attention needed for the account balance fields (S9(13)V99 COMP-3) as financial calculation accuracy is critical for regulatory compliance.

---
**Template version:** 1.0
**Last updated:** 2026-02-15
