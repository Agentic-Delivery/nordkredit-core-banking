---
title: Consolidated Regulatory Traceability Matrix
sidebar_position: 2
---

# Consolidated Regulatory Traceability Matrix

This matrix maps all extracted business rules to their applicable regulatory requirements, providing a single auditable document for FSA/DORA compliance review.

**Last updated:** 2026-02-17
**Total business rules:** 54
**Regulations in scope:** 7

## Coverage Summary

| Regulation | Articles Mapped | Business Rules | Coverage |
|-----------|----------------|---------------|----------|
| FSA FFFS 2014:5 | Ch. 3, Ch. 4 &sect;3, Ch. 6, Ch. 7, Ch. 8 &sect;4 | 46 | High |
| PSD2 | Art. 45, Art. 57, Art. 64, Art. 73, Art. 97, Art. 98 | 40 | High |
| GDPR | Art. 5(1)(c), Art. 5(1)(d), Art. 5(1)(e), Art. 5(1)(f), Art. 5(2), Art. 15, Art. 17, Art. 25 | 22 | Medium |
| AML/KYC (2017:11) | Para. 3, Para. 4 | 14 | Medium |
| DORA | Art. 9, Art. 11, Art. 17, Art. 19 | 14 | Medium |
| EBA Outsourcing Guidelines | General | 1 | Low |
| EU Consumer Credit Directive 2008/48/EC | General | 4 | Low |

---

## FSA FFFS 2014:5

Swedish Financial Supervisory Authority regulations for financial institutions.

### Chapter 3 &mdash; General Governance and Record-Keeping

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| FSA FFFS 2014:5 Ch. 3 | ACCT-BR-001 | CVACT01Y.cpy:1-30 | Account Management | Extracted | Account master data structure and fields |
| FSA FFFS 2014:5 Ch. 3 | ACCT-BR-004 | CBTRN02C.cbl:545-560 | Account Management | Extracted | Account balance and transaction posting |
| FSA FFFS 2014:5 Ch. 3 | RPT-BR-001 | CBTRN03C.cbl:1-650 | Reporting | Extracted | FSA regulatory reporting structure and calendar compliance |
| FSA FFFS 2014:5 Ch. 3 | RPT-BR-003 | CBTRN03C.cbl:158-374 | Reporting | Extracted | Customer statement generation |
| FSA FFFS 2014:5 Ch. 3 | RPT-BR-005 | CBTRN03C.cbl:1-650, CBTRN02C.cbl:1-723 | Reporting | Extracted | Internal management reporting and analytics |
| FSA FFFS 2014:5 Ch. 3 | RPT-BR-007 | CBTRN03C.cbl:324-374 | Reporting | Extracted | Report output formatting and structure standards |

### Chapter 4 &sect;3 &mdash; Operational Risk Management

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| FSA FFFS 2014:5 Ch. 4 &sect;3 | ACCT-BR-002 | COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760 | Account Management | Extracted | Credit limit management and enforcement |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | ACCT-BR-005 | CVACT02Y.cpy:10, COCRDUPC.cbl:845-876, CBTRN02C.cbl:414-420 | Account Management | Extracted | Account status lifecycle |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | ACCT-BR-006 | CBTRN02C.cbl:414-420, COCRDUPC.cbl:913-947 | Account Management | Extracted | Account holder verification and KYC |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | CARD-BR-004 | COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760 | Card Management | Extracted | Card number encryption and secure handling |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | CARD-BR-005 | COCRDSLC.cbl:685-724, COCRDUPC.cbl:762-800 | Card Management | Extracted | Card expiry date management |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | CARD-BR-006 | COCRDUPC.cbl:806-947 | Card Management | Extracted | Card PIN management and reset |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | SEC-BR-004 | COCRDLIC.cbl:346-375, COCRDSLC.cbl:291-299, COCRDUPC.cbl:413-424 | User Security | Extracted | Function key validation and authorized actions |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | SEC-BR-006 | COCRDUPC.cbl:429-543, 948-1027, 988-1001, 469-476 | User Security | Extracted | Data modification authorization and confirmation workflow |
| FSA FFFS 2014:5 Ch. 4 &sect;3 | SEC-BR-007 | COCRDLIC.cbl:263-271, 578-598, COCRDSLC.cbl:250-252, 857-878, COCRDUPC.cbl:370-372, 1019-1026 | User Security | Extracted | Abend handling and secure error management |

### Chapter 6 &mdash; Disclosure and Consumer Credit

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| FSA FFFS 2014:5 Ch. 6 | ACCT-BR-007 | CBTRN02C.cbl:403-413 | Account Management | Extracted | Account credit limit utilization and overdraft rules |
| FSA FFFS 2014:5 Ch. 6 | BILL-BR-001 | CVTRA02Y.cpy:1-12 | Billing | Extracted | Interest rate disclosure and calculation |
| FSA FFFS 2014:5 Ch. 6 | BILL-BR-002 | CBTRN02C.cbl:370-420 | Billing | Extracted | Fee structure and application rules |

### Chapter 7 &mdash; Reporting and Record Retention

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| FSA FFFS 2014:5 Ch. 7 | ACCT-BR-004 | CBTRN02C.cbl:545-560 | Account Management | Extracted | Account balance and transaction posting |
| FSA FFFS 2014:5 Ch. 7 | ACCT-BR-008 | COCRDLIC.cbl:258-260, 847-852, 1167-1175 | Account Management | Extracted | Account query and retrieval patterns |
| FSA FFFS 2014:5 Ch. 7 | BILL-BR-003 | CBTRN02C.cbl:545-560 | Billing | Extracted | Daily interest accrual and balance calculation |
| FSA FFFS 2014:5 Ch. 7 | BILL-BR-004 | CBTRN02C.cbl:467-501 | Billing | Extracted | Late payment detection and notification |
| FSA FFFS 2014:5 Ch. 7 | BILL-BR-006 | CBTRN02C.cbl:193-234, 370-422, 562-579 | Billing | Extracted | Billing cycle close and statement preparation |
| FSA FFFS 2014:5 Ch. 7 | BILL-BR-007 | CBTRN03C.cbl:93-113 | Billing | Extracted | Minimum payment calculation |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-001 | CBTRN03C.cbl:1-650 | Reporting | Extracted | FSA regulatory reporting structure |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-002 | CBTRN03C.cbl:158-374, CBTRN02C.cbl:1-723 | Reporting | Extracted | AML/KYC screening report generation |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-004 | CBTRN03C.cbl:158-374 | Reporting | Extracted | Daily transaction detail report |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-005 | CBTRN03C.cbl:1-650, CBTRN02C.cbl:1-723 | Reporting | Extracted | Internal management reporting |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-006 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | DORA incident reporting and ICT risk management |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-007 | CBTRN03C.cbl:324-374 | Reporting | Extracted | Report output formatting and structure standards |
| FSA FFFS 2014:5 Ch. 7 | RPT-BR-008 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | Batch reporting SLA and scheduling rules |
| FSA FFFS 2014:5 Ch. 7 | TRN-BR-006 | CBTRN03C.cbl:158-374 | Transactions | Extracted | Transaction detail report from daily posting |

### Chapter 8 &sect;4 &mdash; Internal Controls and IT Security

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| FSA FFFS 2014:5 Ch. 8 &sect;4 | CARD-BR-002 | COCRDLIC.cbl:77-82, 1073-1115 | Card Management | Extracted | Card status transitions |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | CARD-BR-007 | COCRDUPC.cbl:275-290, 429-543, 948-1027 | Card Management | Extracted | Card replacement and reissuance |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | CARD-BR-008 | COCRDUPC.cbl:1420-1523 | Card Management | Extracted | Card supplementary cardholder management |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | CARD-BR-011 | COCRDLIC.cbl:384-406, 458-482, COCRDSLC.cbl:268-348, COCRDUPC.cbl:429-543 | Card Management | Extracted | Card fraud detection and monitoring |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-001 | COCRDLIC.cbl:4-7, 315-321, 1382-1410 | User Security | Extracted | Role-based access control |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-002 | COCRDLIC.cbl:1003-1067, 1382-1410, 147-149 | User Security | Extracted | Account-level data isolation and filtering |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-003 | COCRDLIC.cbl:226-262, 315-332, 604-619, COCRDSLC.cbl:198-205, COCRDUPC.cbl:345 | User Security | Extracted | User session context management |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-004 | COCRDLIC.cbl:346-375, 389-517, COCRDSLC.cbl:291-299, 304-381, COCRDUPC.cbl:413-424, 429-543 | User Security | Extracted | Function key validation and authorized actions |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-005 | COCRDLIC.cbl:284-285, COCRDSLC.cbl:226-227, COCRDUPC.cbl:345-346 | User Security | Extracted | CICS terminal authentication and PSD2 SCA mapping |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-006 | COCRDUPC.cbl:429-543, 948-1027, 988-1001, 469-476 | User Security | Extracted | Data modification authorization workflow |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-007 | COCRDLIC.cbl:263-271, 578-598, COCRDSLC.cbl:250-252, 857-878, COCRDUPC.cbl:370-372, 1019-1026 | User Security | Extracted | Abend handling and secure error management |
| FSA FFFS 2014:5 Ch. 8 &sect;4 | SEC-BR-008 | COCRDLIC.cbl:604-608, 522-550, COCRDSLC.cbl:323-334, 393-394, COCRDUPC.cbl:456-476 | User Security | Extracted | Navigation audit trail and program flow tracking |

---

## PSD2 (Payment Services Directive 2)

EU directive on payment services, focusing on strong customer authentication and transaction security.

### Article 45 &mdash; Information on Fees and Charges

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| PSD2 Art. 45 | BILL-BR-001 | CVTRA02Y.cpy:1-12 | Billing | Extracted | Interest rate disclosure and calculation |
| PSD2 Art. 45 | RPT-BR-003 | CBTRN03C.cbl:158-374 | Reporting | Extracted | Customer statement generation |

### Article 57 &mdash; Information for the Payer After Execution

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| PSD2 Art. 57 | RPT-BR-003 | CBTRN03C.cbl:158-374, CVACT01Y.cpy | Reporting | Extracted | Customer statement generation |

### Article 64 &mdash; Execution of Payment Transactions

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| PSD2 Art. 64 | ACCT-BR-001 | CVACT01Y.cpy:1-30 | Account Management | Extracted | Account master data structure |
| PSD2 Art. 64 | ACCT-BR-004 | CBTRN02C.cbl:545-560 | Account Management | Extracted | Account balance and transaction posting |
| PSD2 Art. 64 | ACCT-BR-007 | CBTRN02C.cbl:403-413 | Account Management | Extracted | Credit limit utilization and overdraft |
| PSD2 Art. 64 | BILL-BR-002 | CBTRN02C.cbl:370-420 | Billing | Extracted | Fee structure and application rules |
| PSD2 Art. 64 | BILL-BR-003 | CBTRN02C.cbl:545-560 | Billing | Extracted | Daily interest accrual |
| PSD2 Art. 64 | BILL-BR-004 | CBTRN02C.cbl:467-501 | Billing | Extracted | Late payment detection and notification |
| PSD2 Art. 64 | BILL-BR-005 | CBTRN02C.cbl:414-420, COCRDUPC.cbl:113-123 | Billing | Extracted | Payment application and balance posting |
| PSD2 Art. 64 | TRN-BR-003 | COTRN02C.cbl:106-784 | Transactions | Extracted | Transaction posting and balance updates |
| PSD2 Art. 64 | TRN-BR-004 | CBTRN01C.cbl:154-251 | Transactions | Extracted | Transaction rejection and error handling |
| PSD2 Art. 64 | TRN-BR-005 | CBTRN02C.cbl:193-579 | Transactions | Extracted | Daily transaction file processing |
| PSD2 Art. 64 | TRN-BR-007 | CVTRA05Y.cpy:1-21, CVTRA06Y.cpy:1-22 | Transactions | Extracted | Transaction record structure and fields |
| PSD2 Art. 64 | TRN-BR-009 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Transactions | Extracted | Daily batch transaction processing pipeline |

### Article 73 &mdash; Notification of Unauthorized Transactions

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| PSD2 Art. 73 | TRN-BR-005 | CBTRN02C.cbl:193-579 | Transactions | Extracted | Daily transaction file processing |

### Article 97 &mdash; Strong Customer Authentication (SCA)

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| PSD2 Art. 97 | ACCT-BR-002 | COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760 | Account Management | Extracted | Credit limit management |
| PSD2 Art. 97 | ACCT-BR-003 | COCRDSLC.cbl:779-812, CVACT03Y.cpy:1-11, CVACT02Y.cpy:1-14 | Account Management | Extracted | Card to account association |
| PSD2 Art. 97 | ACCT-BR-005 | CVACT02Y.cpy:10, COCRDUPC.cbl:845-876, CBTRN02C.cbl:414-420 | Account Management | Extracted | Account status lifecycle |
| PSD2 Art. 97 | ACCT-BR-006 | CBTRN02C.cbl:414-420, COCRDUPC.cbl:913-947 | Account Management | Extracted | Account holder verification |
| PSD2 Art. 97 | ACCT-BR-008 | COCRDLIC.cbl:258-260, 847-852, 1167-1175 | Account Management | Extracted | Account query and retrieval |
| PSD2 Art. 97 | CARD-BR-001 | COCRDLIC.cbl:1123-1411 | Card Management | Extracted | Card issuance and activation |
| PSD2 Art. 97 | CARD-BR-002 | COCRDLIC.cbl:77-82, 1073-1115 | Card Management | Extracted | Card status transitions |
| PSD2 Art. 97 | CARD-BR-003 | COCRDSLC.cbl:608-812 | Card Management | Extracted | Card block/unblock operations |
| PSD2 Art. 97 | CARD-BR-005 | COCRDSLC.cbl:685-724, COCRDUPC.cbl:762-800 | Card Management | Extracted | Card expiry date management |
| PSD2 Art. 97 | CARD-BR-006 | COCRDUPC.cbl:806-947 | Card Management | Extracted | Card PIN management |
| PSD2 Art. 97 | CARD-BR-007 | COCRDUPC.cbl:275-290, 429-543, 948-1027 | Card Management | Extracted | Card replacement and reissuance |
| PSD2 Art. 97 | CARD-BR-008 | COCRDUPC.cbl:1420-1523 | Card Management | Extracted | Supplementary cardholder management |
| PSD2 Art. 97 | CARD-BR-009 | CVACT02Y.cpy:1-14 | Card Management | Extracted | Card data retention and purging |
| PSD2 Art. 97 | CARD-BR-011 | COCRDLIC.cbl:384-406, 458-482, COCRDSLC.cbl:268-348, COCRDUPC.cbl:429-543 | Card Management | Extracted | Card fraud detection and monitoring |
| PSD2 Art. 97 | RPT-BR-002 | CBTRN03C.cbl:158-374, CBTRN02C.cbl:1-723 | Reporting | Extracted | AML/KYC screening report |
| PSD2 Art. 97 | TRN-BR-001 | COTRN00C.cbl:94-328 | Transactions | Extracted | Card transaction validation |
| PSD2 Art. 97 | TRN-BR-002 | COTRN01C.cbl:85-296 | Transactions | Extracted | Transaction authorization and limit checking |
| PSD2 Art. 97 | TRN-BR-003 | COTRN02C.cbl:106-784 | Transactions | Extracted | Transaction posting and balance updates |
| PSD2 Art. 97 | TRN-BR-008 | COTRN00C.cbl:62-71, 107-141, 510-521, COTRN01C.cbl:53-61, 94-139, 197-208, COTRN02C.cbl:72-80, 115-159, 500-511 | Transactions | Extracted | Transaction source code mapping |
| PSD2 Art. 97 | SEC-BR-001 | COCRDLIC.cbl:4-7, 315-321, 1382-1410 | User Security | Extracted | Role-based access control |
| PSD2 Art. 97 | SEC-BR-002 | COCRDLIC.cbl:1003-1067, 1382-1410, 147-149 | User Security | Extracted | Account-level data isolation |
| PSD2 Art. 97 | SEC-BR-003 | COCRDLIC.cbl:226-262, 315-332, 604-619, COCRDSLC.cbl:198-205, COCRDUPC.cbl:345 | User Security | Extracted | Session context management |
| PSD2 Art. 97 | SEC-BR-004 | COCRDLIC.cbl:346-375, 389-517, COCRDSLC.cbl:291-299, 304-381, COCRDUPC.cbl:413-424, 429-543 | User Security | Extracted | Function key validation |
| PSD2 Art. 97 | SEC-BR-005 | COCRDLIC.cbl:284-285, COCRDSLC.cbl:226-227, COCRDUPC.cbl:345-346 | User Security | Extracted | Terminal authentication and SCA mapping |
| PSD2 Art. 97 | SEC-BR-006 | COCRDUPC.cbl:429-543, 948-1027, 988-1001, 469-476 | User Security | Extracted | Data modification authorization |
| PSD2 Art. 97 | SEC-BR-008 | COCRDLIC.cbl:604-608, 522-550, COCRDSLC.cbl:323-334, 393-394, COCRDUPC.cbl:456-476 | User Security | Extracted | Navigation audit trail |

### Article 98 &mdash; Dynamic Linking

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| PSD2 Art. 98 | SEC-BR-005 | COCRDLIC.cbl:284-285, COCRDSLC.cbl:226-227, COCRDUPC.cbl:345-346 | User Security | Extracted | CICS terminal authentication and SCA mapping |
| PSD2 Art. 98 | SEC-BR-006 | COCRDUPC.cbl:429-543, 948-1027, 988-1001, 469-476 | User Security | Extracted | Data modification authorization workflow |

---

## GDPR (General Data Protection Regulation)

EU regulation on data protection and privacy for individuals.

### Article 5(1)(c) &mdash; Data Minimisation

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 5(1)(c) | ACCT-BR-001 | CVACT01Y.cpy:1-30 | Account Management | Extracted | Account master data structure |
| GDPR Art. 5(1)(c) | CARD-BR-009 | CVACT02Y.cpy:1-14 | Card Management | Extracted | Card data retention and purging |
| GDPR Art. 5(1)(c) | CARD-BR-010 | CVACT03Y.cpy:1-11 | Card Management | Extracted | Card cross-reference index maintenance |
| GDPR Art. 5(1)(c) | SEC-BR-002 | COCRDLIC.cbl:1003-1067, 1382-1410, 147-149 | User Security | Extracted | Account-level data isolation and filtering |

### Article 5(1)(d) &mdash; Accuracy

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 5(1)(d) | ACCT-BR-001 | CVACT01Y.cpy:1-30 | Account Management | Extracted | Account master data structure |
| GDPR Art. 5(1)(d) | CARD-BR-006 | COCRDUPC.cbl:806-947 | Card Management | Extracted | Card PIN management |
| GDPR Art. 5(1)(d) | CARD-BR-009 | CVACT02Y.cpy:1-14 | Card Management | Extracted | Card data retention and purging |

### Article 5(1)(e) &mdash; Storage Limitation

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 5(1)(e) | ACCT-BR-006 | CBTRN02C.cbl:414-420, COCRDUPC.cbl:913-947 | Account Management | Extracted | Account holder verification and KYC |

### Article 5(1)(f) &mdash; Integrity and Confidentiality

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 5(1)(f) | SEC-BR-001 | COCRDLIC.cbl:4-7, 315-321, 1382-1410 | User Security | Extracted | Role-based access control |
| GDPR Art. 5(1)(f) | SEC-BR-002 | COCRDLIC.cbl:1003-1067, 1382-1410, 147-149 | User Security | Extracted | Account-level data isolation |
| GDPR Art. 5(1)(f) | SEC-BR-003 | COCRDLIC.cbl:226-262, 315-332, 604-619, COCRDSLC.cbl:198-205, COCRDUPC.cbl:345 | User Security | Extracted | Session context management |
| GDPR Art. 5(1)(f) | SEC-BR-007 | COCRDLIC.cbl:263-271, 578-598, COCRDSLC.cbl:250-252, 857-878, COCRDUPC.cbl:370-372, 1019-1026 | User Security | Extracted | Abend handling and secure error management |

### Article 5(2) &mdash; Accountability

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 5(2) | SEC-BR-008 | COCRDLIC.cbl:604-608, 522-550, COCRDSLC.cbl:323-334, 393-394, COCRDUPC.cbl:456-476 | User Security | Extracted | Navigation audit trail and program flow tracking |

### Article 5 (General) &mdash; Data Processing Principles

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 5 | RPT-BR-002 | CBTRN03C.cbl:158-374, CBTRN02C.cbl:1-723 | Reporting | Extracted | AML/KYC screening report generation |
| GDPR Art. 5 | TRN-BR-007 | CVTRA05Y.cpy:1-21, CVTRA06Y.cpy:1-22 | Transactions | Extracted | Transaction record structure and fields |

### Article 15 &mdash; Right of Access

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 15 | ACCT-BR-003 | COCRDSLC.cbl:779-812, CVACT03Y.cpy:1-11, CVACT02Y.cpy:1-14 | Account Management | Extracted | Card to account association |
| GDPR Art. 15 | ACCT-BR-008 | COCRDLIC.cbl:258-260, 847-852, 1167-1175 | Account Management | Extracted | Account query and retrieval |
| GDPR Art. 15 | CARD-BR-001 | COCRDLIC.cbl:1123-1411 | Card Management | Extracted | Card issuance and activation |
| GDPR Art. 15 | CARD-BR-003 | COCRDSLC.cbl:608-812 | Card Management | Extracted | Card block/unblock operations |
| GDPR Art. 15 | RPT-BR-003 | CBTRN03C.cbl:158-374 | Reporting | Extracted | Customer statement generation |
| GDPR Art. 15 | TRN-BR-001 | COTRN00C.cbl:94-328 | Transactions | Extracted | Card transaction validation |
| GDPR Art. 15 | TRN-BR-002 | COTRN01C.cbl:85-296 | Transactions | Extracted | Transaction authorization |

### Article 17 &mdash; Right to Erasure

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 17 | ACCT-BR-005 | CVACT02Y.cpy:10, COCRDUPC.cbl:845-876, CBTRN02C.cbl:414-420 | Account Management | Extracted | Account status lifecycle (closed accounts) |
| GDPR Art. 17 | BILL-BR-005 | CBTRN02C.cbl:414-420, COCRDUPC.cbl:113-123 | Billing | Extracted | Payment application and balance posting |

### Article 25 &mdash; Data Protection by Design

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| GDPR Art. 25 | SEC-BR-001 | COCRDLIC.cbl:4-7, 315-321, 1382-1410 | User Security | Extracted | Role-based access control |
| GDPR Art. 25 | SEC-BR-002 | COCRDLIC.cbl:1003-1067, 1382-1410, 147-149 | User Security | Extracted | Account-level data isolation |

---

## AML/KYC (FFFS 2017:11)

Swedish anti-money laundering and know-your-customer regulations.

### Paragraph 3 &mdash; Customer Due Diligence

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| AML 2017:11 Para. 3 | RPT-BR-002 | CBTRN03C.cbl:158-374, CBTRN02C.cbl:1-723 | Reporting | Extracted | AML/KYC screening report generation |
| AML 2017:11 Para. 3 | RPT-BR-004 | CBTRN03C.cbl:158-374 | Reporting | Extracted | Daily transaction detail report |
| AML 2017:11 Para. 3 | RPT-BR-008 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | Batch reporting SLA and scheduling |
| AML 2017:11 Para. 3 | TRN-BR-006 | CBTRN03C.cbl:158-374 | Transactions | Extracted | Transaction detail report |

### Paragraph 4 &mdash; Suspicious Transaction Monitoring

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| AML 2017:11 Para. 4 | RPT-BR-002 | CBTRN03C.cbl:158-374, CBTRN02C.cbl:1-723 | Reporting | Extracted | AML/KYC screening report generation |

### General AML/KYC References

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| AML 2017:11 | ACCT-BR-003 | COCRDSLC.cbl:779-812, CVACT03Y.cpy:1-11, CVACT02Y.cpy:1-14 | Account Management | Extracted | Card to account association |
| AML 2017:11 | BILL-BR-006 | CBTRN02C.cbl:193-234, 370-422, 562-579 | Billing | Extracted | Billing cycle close and statement preparation |
| AML 2017:11 | BILL-BR-007 | CBTRN03C.cbl:93-113 | Billing | Extracted | Minimum payment calculation |
| AML 2017:11 | CARD-BR-010 | CVACT03Y.cpy:1-11 | Card Management | Extracted | Card cross-reference index maintenance |
| AML 2017:11 | TRN-BR-003 | COTRN02C.cbl:106-784 | Transactions | Extracted | Transaction posting and balance updates |
| AML 2017:11 | TRN-BR-004 | CBTRN01C.cbl:154-251 | Transactions | Extracted | Transaction rejection and error handling |
| AML 2017:11 | TRN-BR-005 | CBTRN02C.cbl:193-579 | Transactions | Extracted | Daily transaction file processing |
| AML 2017:11 | TRN-BR-009 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Transactions | Extracted | Daily batch transaction processing pipeline |

---

## DORA (Digital Operational Resilience Act)

EU regulation on ICT risk management for financial entities.

### Article 9 &mdash; Protection and Prevention

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| DORA Art. 9 | SEC-BR-005 | COCRDLIC.cbl:284-285, COCRDSLC.cbl:226-227, COCRDUPC.cbl:345-346 | User Security | Extracted | CICS terminal authentication and PSD2 SCA mapping |

### Article 11 &mdash; Logging and Detection

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| DORA Art. 11 | ACCT-BR-004 | CBTRN02C.cbl:545-560 | Account Management | Extracted | Account balance and transaction posting |
| DORA Art. 11 | BILL-BR-003 | CBTRN02C.cbl:545-560 | Billing | Extracted | Daily interest accrual |
| DORA Art. 11 | BILL-BR-006 | CBTRN02C.cbl:193-234, 370-422, 562-579 | Billing | Extracted | Billing cycle close |
| DORA Art. 11 | BILL-BR-007 | CBTRN03C.cbl:93-113 | Billing | Extracted | Minimum payment calculation |
| DORA Art. 11 | RPT-BR-004 | CBTRN03C.cbl:158-374 | Reporting | Extracted | Daily transaction detail report |
| DORA Art. 11 | RPT-BR-005 | CBTRN03C.cbl:1-650, CBTRN02C.cbl:1-723 | Reporting | Extracted | Internal management reporting |
| DORA Art. 11 | RPT-BR-006 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | DORA incident reporting |
| DORA Art. 11 | RPT-BR-008 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | Batch reporting SLA |
| DORA Art. 11 | TRN-BR-005 | CBTRN02C.cbl:193-579 | Transactions | Extracted | Daily transaction file processing |
| DORA Art. 11 | TRN-BR-006 | CBTRN03C.cbl:158-374 | Transactions | Extracted | Transaction detail report |
| DORA Art. 11 | TRN-BR-009 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Transactions | Extracted | Daily batch transaction processing |
| DORA Art. 11 | SEC-BR-003 | COCRDLIC.cbl:226-262, 315-332, 604-619, COCRDSLC.cbl:198-205, COCRDUPC.cbl:345 | User Security | Extracted | Session context management |
| DORA Art. 11 | SEC-BR-007 | COCRDLIC.cbl:263-271, 578-598, COCRDSLC.cbl:250-252, 857-878, COCRDUPC.cbl:370-372, 1019-1026 | User Security | Extracted | Abend handling and error management |
| DORA Art. 11 | SEC-BR-008 | COCRDLIC.cbl:604-608, 522-550, COCRDSLC.cbl:323-334, 393-394, COCRDUPC.cbl:456-476 | User Security | Extracted | Navigation audit trail |

### Article 17 &mdash; ICT-Related Incident Management

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| DORA Art. 17 | RPT-BR-006 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | DORA incident reporting and ICT risk management |

### Article 19 &mdash; Reporting of Major ICT-Related Incidents

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| DORA Art. 19 | RPT-BR-006 | CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 | Reporting | Extracted | DORA incident reporting and ICT risk management |

---

## EBA Outsourcing Guidelines

European Banking Authority guidelines on outsourcing arrangements.

### General Requirements

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| EBA Outsourcing Guidelines | RPT-BR-001 | CBTRN03C.cbl:1-650 | Reporting | Extracted | FSA regulatory reporting structure and calendar compliance |

---

## EU Consumer Credit Directive 2008/48/EC

EU directive on credit agreements for consumers.

### General Requirements

| Regulation Article | Business Rule ID | COBOL Source | Domain | Status | Notes |
|-------------------|-----------------|-------------|--------|--------|-------|
| Consumer Credit Directive 2008/48/EC | ACCT-BR-007 | CBTRN02C.cbl:403-413 | Account Management | Extracted | Credit limit utilization and overdraft rules |
| Consumer Credit Directive 2008/48/EC | BILL-BR-001 | CVTRA02Y.cpy:1-12 | Billing | Extracted | Interest rate disclosure and calculation |
| Consumer Credit Directive 2008/48/EC | BILL-BR-002 | CBTRN02C.cbl:370-420 | Billing | Extracted | Fee structure and application rules |
| Consumer Credit Directive 2008/48/EC | BILL-BR-004 | CBTRN02C.cbl:467-501 | Billing | Extracted | Late payment detection and notification |

---

## Coverage Gaps

This section identifies regulatory areas where no business rules have been mapped yet. These represent potential compliance risks that must be addressed as more COBOL programs are analyzed.

### FSA FFFS 2014:5

| Chapter/Section | Topic | Gap Description |
|----------------|-------|-----------------|
| Ch. 5 | Capital adequacy | No business rules extracted for capital adequacy calculations. May be handled outside card management programs. |
| Ch. 9 | Outsourcing | No rules mapped to FSA outsourcing requirements. EBA Outsourcing Guidelines have minimal coverage (1 rule). |
| Ch. 10 | Business continuity | No business continuity rules extracted. COBOL batch programs may contain DR/failover logic not yet analyzed. |

### PSD2

| Article | Topic | Gap Description |
|---------|-------|-----------------|
| Art. 4-5 | Definitions and scope | No explicit scope validation rules extracted. |
| Art. 66-67 | Payment initiation services (PIS) | No rules for third-party PIS access. May be outside current COBOL scope (future open banking). |
| Art. 69 | Account information services (AIS) | No rules for third-party AIS access. May be outside current COBOL scope (future open banking). |
| Art. 74-77 | Liability for unauthorized transactions | No business rules for refund/chargeback processing extracted yet. |

### GDPR

| Article | Topic | Gap Description |
|---------|-------|-----------------|
| Art. 6 | Lawful basis for processing | No explicit rules mapping data processing to a lawful basis. |
| Art. 13-14 | Information to data subjects | No rules for privacy notices or data collection disclosures. |
| Art. 20 | Right to data portability | No rules for data export in machine-readable format. |
| Art. 33-34 | Breach notification | No rules for data breach detection or notification procedures. |
| Art. 35 | Data protection impact assessment | No DPIA rules extracted; may be a process requirement rather than code. |

### AML/KYC (2017:11)

| Paragraph | Topic | Gap Description |
|-----------|-------|-----------------|
| Para. 5-6 | Enhanced due diligence | No rules for politically exposed persons (PEP) screening or enhanced CDD. |
| Para. 7 | Ongoing monitoring | Transaction monitoring rules exist (RPT-BR-002) but no dedicated ongoing CDD rules. |
| Para. 8 | Reporting to FI (Financial Intelligence) | No rules for Suspicious Activity Report (SAR) submission to Finanspolisen. |

### DORA

| Article | Topic | Gap Description |
|---------|-------|-----------------|
| Art. 5-6 | ICT governance | No rules for ICT governance framework. May be organizational rather than code-based. |
| Art. 7-8 | ICT risk management framework | No rules for systematic ICT risk assessment extracted. |
| Art. 10 | Response and recovery | No rules for ICT incident response or system recovery procedures. |
| Art. 12 | Backup policies | No rules for data backup or recovery testing. |
| Art. 24-27 | ICT third-party risk | No rules for third-party ICT provider management. |

### EBA Outsourcing Guidelines

| Section | Topic | Gap Description |
|---------|-------|-----------------|
| General | Outsourcing risk assessment | Only 1 rule mapped (RPT-BR-001). Cloud migration to Azure requires comprehensive outsourcing compliance. |
| General | Exit strategy | No rules for provider exit strategy or data retrieval. Critical for Azure migration. |

### EU Consumer Credit Directive 2008/48/EC

| Article | Topic | Gap Description |
|---------|-------|-----------------|
| Art. 5-6 | Pre-contractual information | No rules for credit agreement pre-contractual disclosures. |
| Art. 10 | Standard information in credit agreements | No rules for standard credit agreement information. |
| Art. 16 | Early repayment | No rules for early repayment rights or calculations. |

---

## Business Rules by Domain (Cross-Reference)

Quick reference showing which regulations apply to each domain.

| Domain | Rule Count | FSA | PSD2 | GDPR | AML | DORA | EBA | CCD |
|--------|-----------|-----|------|------|-----|------|-----|-----|
| Account Management | 8 | 8 | 7 | 5 | 1 | 1 | 0 | 1 |
| Billing | 7 | 7 | 5 | 1 | 2 | 3 | 0 | 3 |
| Card Management | 11 | 6 | 9 | 4 | 1 | 0 | 0 | 0 |
| Reporting | 8 | 8 | 3 | 2 | 4 | 4 | 1 | 0 |
| Transactions | 9 | 3 | 9 | 3 | 4 | 4 | 0 | 0 |
| User Security | 8 | 8 | 7 | 7 | 0 | 4 | 0 | 0 |
| **Total unique rules** | **54** | **46** | **40** | **22** | **14** | **14** | **1** | **4** |

---

## Methodology

This traceability matrix was compiled from the regulatory mapping sections of all 54 extracted business rules. Each business rule document contains a frontmatter `regulations` field and a detailed "Regulatory Mapping" section that maps specific regulation articles to the rule's implementation.

**Data sources:**
- 8 Account Management rules (`ACCT-BR-001` through `ACCT-BR-008`)
- 7 Billing rules (`BILL-BR-001` through `BILL-BR-007`)
- 11 Card Management rules (`CARD-BR-001` through `CARD-BR-011`)
- 8 Reporting rules (`RPT-BR-001` through `RPT-BR-008`)
- 9 Transaction rules (`TRN-BR-001` through `TRN-BR-009`)
- 8 User Security rules (`SEC-BR-001` through `SEC-BR-008`)
- 3 additional Transaction implementation rules (`TRN-BR-003`, `TRN-BR-004`, `TRN-BR-005` detailed versions)

**Validation status:** All rules have status "extracted" (awaiting domain expert validation). No rules have been validated or implemented yet.

**Update procedure:** This matrix must be updated whenever:
1. New business rules are extracted from COBOL source
2. Existing rules are validated by domain experts
3. Implementation status changes
4. New regulatory requirements are identified
