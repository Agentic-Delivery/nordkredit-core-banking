@BDD @Deposits @DEP-BR-005
Feature: Term deposit maturity and renewal processing
    As a bank system
    I want to detect matured term deposits and process renewal instructions
    So that term deposit lifecycle complies with FSA and Deposit Guarantee Directive

    COBOL source: CVACT01Y.cpy (ACCT-EXPIRAION-DATE), CBTRN02C.cbl:414-420.
    Business rule: DEP-BR-005 (term deposit maturity and renewal processing).
    Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive, PSD2 Art. 57.

    # ===================================================================
    # DEP-BR-005: Maturity detection
    # COBOL: ACCT-EXPIRAION-DATE PIC X(10), format YYYY-MM-DD
    # Returns true when maturity date has passed
    # ===================================================================

    Scenario: Term deposit is matured when maturity date has passed
        Given a deposit account with maturity date "2025-06-15"
        When I check maturity as of "2025-06-16"
        Then the account is matured

    Scenario: Term deposit is not matured on maturity date
        Given a deposit account with maturity date "2025-06-15"
        When I check maturity as of "2025-06-15"
        Then the account is not matured

    Scenario: Term deposit is not matured before maturity date
        Given a deposit account with maturity date "2025-06-15"
        When I check maturity as of "2025-06-14"
        Then the account is not matured

    Scenario: Demand deposit without maturity date is never matured
        Given a deposit account without a maturity date
        When I check maturity as of "2025-06-16"
        Then the account is not matured

    # ===================================================================
    # DEP-BR-005: Term deposit metadata
    # ===================================================================

    Scenario: Term deposit records renewal instruction
        Given a term deposit for account "12345678901" with
            | TermMonths | FixedRate | PrincipalAmount | RenewalInstruction | StartDate  |
            | 12         | 0.035     | 100000.00       | AutoRenew          | 2024-06-15 |
        Then the term deposit has term months 12
        And the term deposit has fixed rate 0.035
        And the term deposit has principal amount 100000.00
        And the term deposit has renewal instruction "AutoRenew"

    Scenario: Term deposit with payout instruction has linked account
        Given a term deposit for account "12345678901" with
            | TermMonths | FixedRate | PrincipalAmount | RenewalInstruction | LinkedAccountId | StartDate  |
            | 6          | 0.028     | 50000.00        | Payout             | 99999999999     | 2025-01-01 |
        Then the term deposit has renewal instruction "Payout"
        And the term deposit has linked account ID "99999999999"
