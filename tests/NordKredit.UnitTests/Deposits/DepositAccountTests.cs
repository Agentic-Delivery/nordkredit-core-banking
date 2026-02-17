using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the DepositAccount entity.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: DEP-BR-001 (data structure), DEP-BR-003 (balance update).
/// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 5(1)(c)(d), Deposit Guarantee Directive.
/// </summary>
public class DepositAccountTests
{
    // =================================================================
    // DEP-BR-001: Deposit account data structure
    // =================================================================

    [Fact]
    public void DepositAccount_ShouldStoreAllFields()
    {
        var account = new DepositAccount
        {
            Id = "12345678901",
            Status = DepositAccountStatus.Active,
            ProductType = DepositProductType.DemandSavings,
            CurrentBalance = 50000.75m,
            CurrentCycleCredit = 10000.00m,
            CurrentCycleDebit = -2000.25m,
            DisclosureGroupId = "SAV-STD",
            MaturityDate = new DateTime(2028, 12, 31),
            HolderName = "JOHN DOE",
            OpenedDate = new DateTime(2020, 1, 15),
            ClosedDate = null,
            AccruedInterest = 123.4567m,
            LastInterestPostingDate = new DateTime(2026, 1, 31),
            RowVersion = [0x00, 0x00, 0x00, 0x01]
        };

        Assert.Equal("12345678901", account.Id);
        Assert.Equal(DepositAccountStatus.Active, account.Status);
        Assert.Equal(DepositProductType.DemandSavings, account.ProductType);
        Assert.Equal(50000.75m, account.CurrentBalance);
        Assert.Equal(10000.00m, account.CurrentCycleCredit);
        Assert.Equal(-2000.25m, account.CurrentCycleDebit);
        Assert.Equal("SAV-STD", account.DisclosureGroupId);
        Assert.Equal(new DateTime(2028, 12, 31), account.MaturityDate);
        Assert.Equal("JOHN DOE", account.HolderName);
        Assert.Equal(new DateTime(2020, 1, 15), account.OpenedDate);
        Assert.Null(account.ClosedDate);
        Assert.Equal(123.4567m, account.AccruedInterest);
        Assert.Equal(new DateTime(2026, 1, 31), account.LastInterestPostingDate);
        Assert.Equal([0x00, 0x00, 0x00, 0x01], account.RowVersion);
    }

    [Fact]
    public void DepositAccount_Id_ShouldBe11Characters()
    {
        // COBOL: ACCT-ID PIC 9(11) — 11-digit account identifier
        var account = new DepositAccount { Id = "12345678901" };

        Assert.Equal(11, account.Id.Length);
    }

    [Fact]
    public void DepositAccount_HolderName_ShouldSupportSwedishCharacters()
    {
        // EBCDIC→UTF-8: Swedish characters (Å, Ä, Ö) must be preserved
        var account = new DepositAccount { HolderName = "BJÖRK ÅSTRÖM" };

        Assert.Contains("Ö", account.HolderName);
        Assert.Contains("Å", account.HolderName);
    }

    [Fact]
    public void DepositAccount_StringProperties_DefaultToEmpty()
    {
        var account = new DepositAccount();

        Assert.Equal(string.Empty, account.Id);
        Assert.Equal(string.Empty, account.HolderName);
        Assert.Equal(string.Empty, account.DisclosureGroupId);
    }

    [Fact]
    public void DepositAccount_RowVersion_DefaultsToEmptyArray()
    {
        var account = new DepositAccount();

        Assert.Empty(account.RowVersion);
    }

    [Fact]
    public void DepositAccount_Defaults_StatusIsActive()
    {
        var account = new DepositAccount();

        Assert.Equal(DepositAccountStatus.Active, account.Status);
    }

    [Fact]
    public void DepositAccount_Defaults_ProductTypeIsDemandSavings()
    {
        var account = new DepositAccount();

        Assert.Equal(DepositProductType.DemandSavings, account.ProductType);
    }

    [Fact]
    public void DepositAccount_FinancialFields_UseDecimal()
    {
        // COBOL: PIC S9(10)V99 — must use decimal, never float/double
        var account = new DepositAccount
        {
            CurrentBalance = 9999999999.99m,
            CurrentCycleCredit = 9999999999.99m,
            CurrentCycleDebit = -9999999999.99m,
            AccruedInterest = 9999.9999m
        };

        Assert.Equal(9999999999.99m, account.CurrentBalance);
        Assert.Equal(9999999999.99m, account.CurrentCycleCredit);
        Assert.Equal(-9999999999.99m, account.CurrentCycleDebit);
        Assert.Equal(9999.9999m, account.AccruedInterest);
    }

    [Fact]
    public void DepositAccount_ZeroCreditLimits_DistinguishFromLending()
    {
        // DEP-BR-001: Pure deposit accounts have zero credit limits
        var account = new DepositAccount();

        Assert.Equal(0m, account.CurrentBalance);
        Assert.Equal(0m, account.CurrentCycleCredit);
        Assert.Equal(0m, account.CurrentCycleDebit);
    }

    // =================================================================
    // DEP-BR-003: Deposit posting and balance update
    // COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC)
    // =================================================================

    [Fact]
    public void ApplyDeposit_PositiveAmount_IncreasesBalanceAndCycleCredit()
    {
        // COBOL: ADD DALYTRAN-AMT TO ACCT-CURR-BAL / ACCT-CURR-CYC-CREDIT
        var account = new DepositAccount
        {
            CurrentBalance = 50000.00m,
            CurrentCycleCredit = 10000.00m
        };

        account.ApplyTransaction(5000.00m);

        Assert.Equal(55000.00m, account.CurrentBalance);
        Assert.Equal(15000.00m, account.CurrentCycleCredit);
    }

    [Fact]
    public void ApplyWithdrawal_NegativeAmount_DecreasesBalanceAndCycleDebit()
    {
        // COBOL: ADD DALYTRAN-AMT TO ACCT-CURR-BAL / ACCT-CURR-CYC-DEBIT
        var account = new DepositAccount
        {
            CurrentBalance = 50000.00m,
            CurrentCycleDebit = -2000.00m
        };

        account.ApplyTransaction(-3000.00m);

        Assert.Equal(47000.00m, account.CurrentBalance);
        Assert.Equal(-5000.00m, account.CurrentCycleDebit);
    }

    [Fact]
    public void ApplyTransaction_ZeroAmount_GoesToCredit()
    {
        // COBOL: IF DALYTRAN-AMT >= 0 — zero is treated as credit
        var account = new DepositAccount { CurrentCycleCredit = 100.00m };

        account.ApplyTransaction(0.00m);

        Assert.Equal(100.00m, account.CurrentCycleCredit);
    }

    [Fact]
    public void ApplyTransaction_PreservesDecimalPrecision()
    {
        // COBOL: PIC S9(10)V99 — must preserve 2-decimal precision
        var account = new DepositAccount { CurrentBalance = 999999999.99m };

        account.ApplyTransaction(0.01m);

        Assert.Equal(1000000000.00m, account.CurrentBalance);
    }

    // =================================================================
    // DEP-BR-005: Maturity date enforcement
    // COBOL source: CBTRN02C.cbl:414-420
    // =================================================================

    [Fact]
    public void IsMatured_BeforeMaturityDate_ReturnsFalse()
    {
        var account = new DepositAccount { MaturityDate = new DateTime(2028, 12, 31) };

        Assert.False(account.IsMatured(new DateTime(2028, 1, 1)));
    }

    [Fact]
    public void IsMatured_AfterMaturityDate_ReturnsTrue()
    {
        var account = new DepositAccount { MaturityDate = new DateTime(2025, 6, 15) };

        Assert.True(account.IsMatured(new DateTime(2025, 6, 16)));
    }

    [Fact]
    public void IsMatured_NullMaturityDate_ReturnsFalse()
    {
        // Demand deposits have no maturity date — never mature
        var account = new DepositAccount { MaturityDate = null };

        Assert.False(account.IsMatured(DateTime.MaxValue));
    }

    [Fact]
    public void IsMatured_OnMaturityDate_ReturnsFalse()
    {
        // COBOL: ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS — equal means not matured
        var account = new DepositAccount { MaturityDate = new DateTime(2025, 6, 15) };

        Assert.False(account.IsMatured(new DateTime(2025, 6, 15)));
    }

    // =================================================================
    // Status transitions
    // =================================================================

    [Fact]
    public void TransitionTo_ActiveToDormant_Succeeds()
    {
        var account = new DepositAccount { Status = DepositAccountStatus.Active };

        var result = account.TransitionTo(DepositAccountStatus.Dormant);

        Assert.True(result.IsValid);
        Assert.Equal(DepositAccountStatus.Dormant, account.Status);
    }

    [Fact]
    public void TransitionTo_ActiveToFrozen_Succeeds()
    {
        var account = new DepositAccount { Status = DepositAccountStatus.Active };

        var result = account.TransitionTo(DepositAccountStatus.Frozen);

        Assert.True(result.IsValid);
        Assert.Equal(DepositAccountStatus.Frozen, account.Status);
    }

    [Fact]
    public void TransitionTo_ActiveToClosed_SetsClosedDate()
    {
        var account = new DepositAccount { Status = DepositAccountStatus.Active };

        var result = account.TransitionTo(DepositAccountStatus.Closed);

        Assert.True(result.IsValid);
        Assert.Equal(DepositAccountStatus.Closed, account.Status);
        Assert.NotNull(account.ClosedDate);
    }

    [Fact]
    public void TransitionTo_DormantToActive_Succeeds()
    {
        var account = new DepositAccount { Status = DepositAccountStatus.Dormant };

        var result = account.TransitionTo(DepositAccountStatus.Active);

        Assert.True(result.IsValid);
        Assert.Equal(DepositAccountStatus.Active, account.Status);
    }

    [Fact]
    public void TransitionTo_FrozenToActive_Succeeds()
    {
        var account = new DepositAccount { Status = DepositAccountStatus.Frozen };

        var result = account.TransitionTo(DepositAccountStatus.Active);

        Assert.True(result.IsValid);
        Assert.Equal(DepositAccountStatus.Active, account.Status);
    }

    [Fact]
    public void TransitionTo_ClosedToAnything_Fails()
    {
        // Closed is terminal — no transitions allowed
        var account = new DepositAccount { Status = DepositAccountStatus.Closed };

        var result = account.TransitionTo(DepositAccountStatus.Active);

        Assert.False(result.IsValid);
        Assert.Equal("Cannot transition from Closed status — terminal state", result.ErrorMessage);
        Assert.Equal(DepositAccountStatus.Closed, account.Status);
    }

    [Fact]
    public void TransitionTo_SameStatus_Fails()
    {
        var account = new DepositAccount { Status = DepositAccountStatus.Active };

        var result = account.TransitionTo(DepositAccountStatus.Active);

        Assert.False(result.IsValid);
        Assert.Equal("Account is already in the requested status", result.ErrorMessage);
    }

    // =================================================================
    // DEP-BR-004: Interest posting
    // =================================================================

    [Fact]
    public void PostInterest_AddsToBalanceAndCycleCredit()
    {
        var account = new DepositAccount
        {
            CurrentBalance = 50000.00m,
            CurrentCycleCredit = 10000.00m,
            AccruedInterest = 205.48m
        };

        account.PostInterest();

        Assert.Equal(50205.48m, account.CurrentBalance);
        Assert.Equal(10205.48m, account.CurrentCycleCredit);
        Assert.Equal(0m, account.AccruedInterest);
    }

    [Fact]
    public void PostInterest_ZeroAccrued_NoChange()
    {
        var account = new DepositAccount
        {
            CurrentBalance = 50000.00m,
            AccruedInterest = 0m
        };

        account.PostInterest();

        Assert.Equal(50000.00m, account.CurrentBalance);
    }

    [Fact]
    public void AccrueInterest_AddsToAccruedBalance()
    {
        var account = new DepositAccount { AccruedInterest = 100.0000m };

        account.AccrueInterest(6.8493m);

        Assert.Equal(106.8493m, account.AccruedInterest);
    }
}
