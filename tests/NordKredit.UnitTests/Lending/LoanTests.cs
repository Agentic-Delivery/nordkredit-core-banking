using NordKredit.Domain.Lending;

namespace NordKredit.UnitTests.Lending;

/// <summary>
/// Tests for the Loan entity.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: LND-BR-001 (data structure), LND-BR-002 (credit limit enforcement),
///                 LND-BR-005 (repayment processing), LND-BR-009 (expiration enforcement).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive, PSD2 Art. 64.
/// </summary>
public class LoanTests
{
    // =================================================================
    // LND-BR-001: Loan account data structure
    // =================================================================

    [Fact]
    public void Loan_ShouldStoreAllFields()
    {
        var loan = new Loan
        {
            AccountId = "12345678901",
            ActiveStatus = LoanStatus.Active,
            LoanType = LoanType.RevolvingCredit,
            CurrentBalance = 25000.50m,
            CreditLimit = 50000.00m,
            CashCreditLimit = 10000.00m,
            CurrentCycleCredit = 5000.00m,
            CurrentCycleDebit = -2000.00m,
            ExpirationDate = new DateTime(2030, 12, 31),
            DisclosureGroupId = "GRP-STD",
            BorrowerName = "ERIK LINDSTRÖM",
            OriginationDate = new DateTime(2024, 1, 15),
            RowVersion = [0x00, 0x00, 0x00, 0x01]
        };

        Assert.Equal("12345678901", loan.AccountId);
        Assert.Equal(LoanStatus.Active, loan.ActiveStatus);
        Assert.Equal(LoanType.RevolvingCredit, loan.LoanType);
        Assert.Equal(25000.50m, loan.CurrentBalance);
        Assert.Equal(50000.00m, loan.CreditLimit);
        Assert.Equal(10000.00m, loan.CashCreditLimit);
        Assert.Equal(5000.00m, loan.CurrentCycleCredit);
        Assert.Equal(-2000.00m, loan.CurrentCycleDebit);
        Assert.Equal(new DateTime(2030, 12, 31), loan.ExpirationDate);
        Assert.Equal("GRP-STD", loan.DisclosureGroupId);
        Assert.Equal("ERIK LINDSTRÖM", loan.BorrowerName);
        Assert.Equal(new DateTime(2024, 1, 15), loan.OriginationDate);
        Assert.Equal([0x00, 0x00, 0x00, 0x01], loan.RowVersion);
    }

    [Fact]
    public void Loan_AccountId_ShouldBe11Characters()
    {
        // COBOL: ACCT-ID PIC 9(11) — 11-digit account identifier
        var loan = new Loan { AccountId = "12345678901" };

        Assert.Equal(11, loan.AccountId.Length);
    }

    [Fact]
    public void Loan_BorrowerName_ShouldSupportSwedishCharacters()
    {
        // EBCDIC→UTF-8: Swedish characters (Å, Ä, Ö) must be preserved
        var loan = new Loan { BorrowerName = "BJÖRK ÅSTRÖM" };

        Assert.Contains("Ö", loan.BorrowerName);
        Assert.Contains("Å", loan.BorrowerName);
    }

    [Fact]
    public void Loan_StringProperties_DefaultToEmpty()
    {
        var loan = new Loan();

        Assert.Equal(string.Empty, loan.AccountId);
        Assert.Equal(string.Empty, loan.DisclosureGroupId);
        Assert.Equal(string.Empty, loan.BorrowerName);
    }

    [Fact]
    public void Loan_RowVersion_DefaultsToEmptyArray()
    {
        var loan = new Loan();

        Assert.Empty(loan.RowVersion);
    }

    [Fact]
    public void Loan_Defaults_StatusIsActive()
    {
        var loan = new Loan();

        Assert.Equal(LoanStatus.Active, loan.ActiveStatus);
    }

    [Fact]
    public void Loan_Defaults_LoanTypeIsRevolvingCredit()
    {
        var loan = new Loan();

        Assert.Equal(LoanType.RevolvingCredit, loan.LoanType);
    }

    [Fact]
    public void Loan_FinancialFields_DefaultToZero()
    {
        var loan = new Loan();

        Assert.Equal(0m, loan.CurrentBalance);
        Assert.Equal(0m, loan.CreditLimit);
        Assert.Equal(0m, loan.CashCreditLimit);
        Assert.Equal(0m, loan.CurrentCycleCredit);
        Assert.Equal(0m, loan.CurrentCycleDebit);
    }

    [Fact]
    public void Loan_CreditLimit_PreservesMaxCobolValue()
    {
        // COBOL: PIC S9(10)V99 — max value 9,999,999,999.99
        var loan = new Loan { CreditLimit = 9999999999.99m };

        Assert.Equal(9999999999.99m, loan.CreditLimit);
    }

    [Fact]
    public void Loan_CurrentBalance_AllowsNegativeValues()
    {
        // COBOL: PIC S9(10)V99 — signed field, negative = overpayment
        var loan = new Loan { CurrentBalance = -500.00m };

        Assert.Equal(-500.00m, loan.CurrentBalance);
    }

    // =================================================================
    // LND-BR-002: Credit limit enforcement
    // COBOL source: CBTRN02C.cbl:403-413
    // =================================================================

    [Fact]
    public void WouldExceedCreditLimit_WithinLimit_ReturnsFalse()
    {
        // Scenario 1: 3000 - 1000 + 5000 = 7000 <= 10000
        var loan = new Loan
        {
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 3000.00m,
            CurrentCycleDebit = -1000.00m
        };

        Assert.False(loan.WouldExceedCreditLimit(5000.00m));
    }

    [Fact]
    public void WouldExceedCreditLimit_ExceedsLimit_ReturnsTrue()
    {
        // Scenario 2: 8000 - (-500) + 3000 = 8000 + 500 + 3000 = 11500 > 10000
        // Wait — the COBOL formula: CycleCredit - CycleDebit + Amount
        // CycleDebit is negative (-500), so: 8000 - (-500) + 3000 = 11500 > 10000
        var loan = new Loan
        {
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 8000.00m,
            CurrentCycleDebit = -500.00m
        };

        Assert.True(loan.WouldExceedCreditLimit(3000.00m));
    }

    [Fact]
    public void WouldExceedCreditLimit_ExactlyAtLimit_ReturnsFalse()
    {
        // Scenario 3: 5000 - 0 + 5000 = 10000 = limit (boundary: equal is allowed)
        var loan = new Loan
        {
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 5000.00m,
            CurrentCycleDebit = 0.00m
        };

        Assert.False(loan.WouldExceedCreditLimit(5000.00m));
    }

    [Fact]
    public void WouldExceedCreditLimit_NegativeTransaction_AlwaysPasses()
    {
        // Scenario 4: Payment (negative) always passes — reduces projected balance
        var loan = new Loan
        {
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 10000.00m,
            CurrentCycleDebit = 0.00m
        };

        Assert.False(loan.WouldExceedCreditLimit(-2000.00m));
    }

    [Fact]
    public void WouldExceedCreditLimit_UsesCycleBalances_NotCurrentBalance()
    {
        // Scenario 5: Overlimit check uses cycle values, not ACCT-CURR-BAL
        // Even though CurrentBalance exceeds CreditLimit, cycle values are within limit
        var loan = new Loan
        {
            CurrentBalance = 15000.00m,
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 2000.00m,
            CurrentCycleDebit = 0.00m
        };

        // Projected: 2000 - 0 + 7000 = 9000 <= 10000
        Assert.False(loan.WouldExceedCreditLimit(7000.00m));
    }

    // =================================================================
    // LND-BR-005: Repayment processing (balance update)
    // COBOL source: CBTRN02C.cbl:545-560
    // =================================================================

    [Fact]
    public void ApplyTransaction_CreditAmount_IncreasesBalanceAndCycleCredit()
    {
        // COBOL: ADD DALYTRAN-AMT TO ACCT-CURR-BAL / ACCT-CURR-CYC-CREDIT
        var loan = new Loan
        {
            CurrentBalance = 5000.00m,
            CurrentCycleCredit = 3000.00m
        };

        loan.ApplyTransaction(2000.00m);

        Assert.Equal(7000.00m, loan.CurrentBalance);
        Assert.Equal(5000.00m, loan.CurrentCycleCredit);
    }

    [Fact]
    public void ApplyTransaction_Repayment_DecreasesBalanceAndUpdatesCycleDebit()
    {
        // Scenario 1: Standard repayment reduces balance
        var loan = new Loan
        {
            CurrentBalance = 25000.00m,
            CurrentCycleCredit = 5000.00m,
            CurrentCycleDebit = 0.00m
        };

        loan.ApplyTransaction(-3000.00m);

        Assert.Equal(22000.00m, loan.CurrentBalance);
        Assert.Equal(-3000.00m, loan.CurrentCycleDebit);
        Assert.Equal(5000.00m, loan.CurrentCycleCredit); // unchanged
    }

    [Fact]
    public void ApplyTransaction_Overpayment_CreatesNegativeBalance()
    {
        // Scenario 2: Overpayment creates negative balance
        var loan = new Loan
        {
            CurrentBalance = 1000.00m,
            CurrentCycleDebit = 0.00m
        };

        loan.ApplyTransaction(-1500.00m);

        Assert.Equal(-500.00m, loan.CurrentBalance);
        Assert.Equal(-1500.00m, loan.CurrentCycleDebit);
    }

    [Fact]
    public void ApplyTransaction_MultipleRepayments_AccumulateInCycleDebit()
    {
        // Scenario 4: Multiple repayments accumulate
        var loan = new Loan
        {
            CurrentBalance = 50000.00m,
            CurrentCycleDebit = -2000.00m
        };

        loan.ApplyTransaction(-3000.00m);

        Assert.Equal(47000.00m, loan.CurrentBalance);
        Assert.Equal(-5000.00m, loan.CurrentCycleDebit);
    }

    [Fact]
    public void ApplyTransaction_ZeroAmount_GoesToCredit()
    {
        // COBOL: IF DALYTRAN-AMT >= 0 — zero is treated as credit
        var loan = new Loan { CurrentCycleCredit = 100.00m };

        loan.ApplyTransaction(0.00m);

        Assert.Equal(100.00m, loan.CurrentCycleCredit);
    }

    [Fact]
    public void ApplyTransaction_PreservesDecimalPrecision()
    {
        // COBOL: PIC S9(10)V99 — must preserve 2-decimal precision
        var loan = new Loan { CurrentBalance = 999999999.99m };

        loan.ApplyTransaction(0.01m);

        Assert.Equal(1000000000.00m, loan.CurrentBalance);
    }

    // =================================================================
    // LND-BR-009: Account expiration enforcement
    // COBOL source: CBTRN02C.cbl:414-420
    // =================================================================

    [Fact]
    public void IsExpired_BeforeExpirationDate_ReturnsFalse()
    {
        var loan = new Loan { ExpirationDate = new DateTime(2030, 12, 31) };

        Assert.False(loan.IsExpired(new DateTime(2030, 1, 1)));
    }

    [Fact]
    public void IsExpired_AfterExpirationDate_ReturnsTrue()
    {
        var loan = new Loan { ExpirationDate = new DateTime(2025, 6, 15) };

        Assert.True(loan.IsExpired(new DateTime(2025, 6, 16)));
    }

    [Fact]
    public void IsExpired_NullExpirationDate_ReturnsFalse()
    {
        // Null means no expiration — loan never expires (revolving credit)
        var loan = new Loan { ExpirationDate = null };

        Assert.False(loan.IsExpired(DateTime.MaxValue));
    }

    [Fact]
    public void IsExpired_OnExpirationDate_ReturnsFalse()
    {
        // COBOL: ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS — equal means not expired
        var loan = new Loan { ExpirationDate = new DateTime(2025, 6, 15) };

        Assert.False(loan.IsExpired(new DateTime(2025, 6, 15)));
    }

    // =================================================================
    // LND-BR-001: Available credit calculation
    // =================================================================

    [Fact]
    public void AvailableCredit_ReturnsCorrectValue()
    {
        // Available Credit = CreditLimit - (CycleCredit - CycleDebit)
        var loan = new Loan
        {
            CreditLimit = 50000.00m,
            CurrentCycleCredit = 10000.00m,
            CurrentCycleDebit = -3000.00m
        };

        // 50000 - (10000 - (-3000)) = 50000 - 13000 = 37000
        Assert.Equal(37000.00m, loan.AvailableCredit);
    }

    [Fact]
    public void AvailableCredit_ZeroCycleBalances_EqualsFullLimit()
    {
        var loan = new Loan { CreditLimit = 50000.00m };

        Assert.Equal(50000.00m, loan.AvailableCredit);
    }

    // =================================================================
    // Loan status transitions
    // =================================================================

    [Fact]
    public void TransitionTo_ActiveToDelinquent_Succeeds()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.Active };

        var result = loan.TransitionTo(LoanStatus.Delinquent);

        Assert.True(result.IsValid);
        Assert.Equal(LoanStatus.Delinquent, loan.ActiveStatus);
    }

    [Fact]
    public void TransitionTo_ActiveToFrozen_Succeeds()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.Active };

        var result = loan.TransitionTo(LoanStatus.Frozen);

        Assert.True(result.IsValid);
        Assert.Equal(LoanStatus.Frozen, loan.ActiveStatus);
    }

    [Fact]
    public void TransitionTo_ActiveToPaidOff_Succeeds()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.Active };

        var result = loan.TransitionTo(LoanStatus.PaidOff);

        Assert.True(result.IsValid);
        Assert.Equal(LoanStatus.PaidOff, loan.ActiveStatus);
    }

    [Fact]
    public void TransitionTo_DelinquentToDefaulted_Succeeds()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.Delinquent };

        var result = loan.TransitionTo(LoanStatus.Defaulted);

        Assert.True(result.IsValid);
        Assert.Equal(LoanStatus.Defaulted, loan.ActiveStatus);
    }

    [Fact]
    public void TransitionTo_ClosedToAnything_Fails()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.Closed };

        var result = loan.TransitionTo(LoanStatus.Active);

        Assert.False(result.IsValid);
        Assert.Contains("Cannot transition from Closed status", result.ErrorMessage);
        Assert.Equal(LoanStatus.Closed, loan.ActiveStatus);
    }

    [Fact]
    public void TransitionTo_SameStatus_Fails()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.Active };

        var result = loan.TransitionTo(LoanStatus.Active);

        Assert.False(result.IsValid);
        Assert.Contains("already in the requested status", result.ErrorMessage);
    }

    [Fact]
    public void TransitionTo_ActiveToDefaulted_Fails()
    {
        // Cannot skip from Active directly to Defaulted — must go through Delinquent
        var loan = new Loan { ActiveStatus = LoanStatus.Active };

        var result = loan.TransitionTo(LoanStatus.Defaulted);

        Assert.False(result.IsValid);
    }

    [Fact]
    public void TransitionTo_PaidOffToClosed_Succeeds()
    {
        var loan = new Loan { ActiveStatus = LoanStatus.PaidOff };

        var result = loan.TransitionTo(LoanStatus.Closed);

        Assert.True(result.IsValid);
        Assert.Equal(LoanStatus.Closed, loan.ActiveStatus);
    }
}
