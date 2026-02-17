@BDD @Lending @LND-BR-003
Feature: Loan origination and credit assessment
    As a bank system
    I want to process loan applications with AML/KYC and creditworthiness checks
    So that lending decisions comply with Consumer Credit Directive and AML regulations

    COBOL source: Dedicated program not yet in repository (inferred from CVACT01Y.cpy).
    Business rule: LND-BR-003 (loan origination and credit assessment).
    Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8 (creditworthiness),
                 AML 2017:11 (customer due diligence), GDPR Art. 6(1)(b).

    # ===================================================================
    # Loan application data structure
    # Consumer Credit Directive Art. 8 — creditworthiness assessment
    # ===================================================================

    Scenario: Loan application created with pending status
        When I submit a loan application for customer "CUST001" requesting 500000.00 for 360 months as Mortgage
        Then the application status is "Pending"
        And AML/KYC has not been verified
        And credit assessment has not been performed

    Scenario: Approved application records credit limit
        Given a loan application for customer "CUST001" requesting 500000.00
        When the application is approved with credit limit 450000.00
        Then the application status is "Approved"
        And the approved credit limit is 450000.00

    Scenario: Rejected application records rejection reason
        Given a loan application for customer "CUST001" requesting 500000.00
        When the application is rejected with reason "Insufficient creditworthiness per FSA FFFS 2014:5"
        Then the application status is "Rejected"
        And the rejection reason is "Insufficient creditworthiness per FSA FFFS 2014:5"

    Scenario: Cancelled application before decision
        Given a loan application for customer "CUST001" requesting 500000.00
        When the application is cancelled
        Then the application status is "Cancelled"
