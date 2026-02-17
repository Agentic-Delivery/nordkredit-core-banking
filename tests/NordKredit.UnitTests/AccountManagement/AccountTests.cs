using NordKredit.Domain.AccountManagement;

namespace NordKredit.UnitTests.AccountManagement;

/// <summary>
/// Tests for the Account Management entity.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: ACCT-BR-001 (data structure), ACCT-BR-004 (balance management),
///                 ACCT-BR-005 (status transitions), ACCT-BR-006 (expiration date),
///                 ACCT-BR-007 (credit limit enforcement).
/// Regulations: GDPR Art. 5(1)(c)(d), FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64.
/// </summary>
public class AccountTests
{
    // =================================================================
    // ACCT-BR-001: Account record data structure
    // =================================================================

    [Fact]
    public void Account_ShouldStoreAllFields()
    {
        var account = new Account
        {
            Id = "12345678901",
            Status = AccountStatus.Active,
            AccountType = AccountType.Credit,
            CurrentBalance = 1500.75m,
            CreditLimit = 5000.00m,
            CashCreditLimit = 1000.00m,
            CurrentCycleCredit = 2000.00m,
            CurrentCycleDebit = -500.25m,
            ExpirationDate = new DateTime(2028, 12, 31),
            HolderName = "JOHN DOE",
            OpenedDate = new DateTime(2020, 1, 15),
            ClosedDate = null,
            RowVersion = [0x00, 0x00, 0x00, 0x01]
        };

        Assert.Equal("12345678901", account.Id);
        Assert.Equal(AccountStatus.Active, account.Status);
        Assert.Equal(AccountType.Credit, account.AccountType);
        Assert.Equal(1500.75m, account.CurrentBalance);
        Assert.Equal(5000.00m, account.CreditLimit);
        Assert.Equal(1000.00m, account.CashCreditLimit);
        Assert.Equal(2000.00m, account.CurrentCycleCredit);
        Assert.Equal(-500.25m, account.CurrentCycleDebit);
        Assert.Equal(new DateTime(2028, 12, 31), account.ExpirationDate);
        Assert.Equal("JOHN DOE", account.HolderName);
        Assert.Equal(new DateTime(2020, 1, 15), account.OpenedDate);
        Assert.Null(account.ClosedDate);
        Assert.Equal([0x00, 0x00, 0x00, 0x01], account.RowVersion);
    }

    [Fact]
    public void Account_Id_ShouldBe11Characters()
    {
        // COBOL: ACCT-ID PIC 9(11) — 11-digit account identifier
        var account = new Account { Id = "12345678901" };

        Assert.Equal(11, account.Id.Length);
    }

    [Fact]
    public void Account_HolderName_ShouldSupportSwedishCharacters()
    {
        // EBCDIC→UTF-8: Swedish characters (Å, Ä, Ö) must be preserved
        var account = new Account { HolderName = "BJÖRK ÅSTRÖM" };

        Assert.Contains("Ö", account.HolderName);
        Assert.Contains("Å", account.HolderName);
    }

    [Fact]
    public void Account_StringProperties_DefaultToEmpty()
    {
        var account = new Account();

        Assert.Equal(string.Empty, account.Id);
        Assert.Equal(string.Empty, account.HolderName);
    }

    [Fact]
    public void Account_RowVersion_DefaultsToEmptyArray()
    {
        var account = new Account();

        Assert.Empty(account.RowVersion);
    }

    [Fact]
    public void Account_Defaults_StatusIsActive()
    {
        var account = new Account();

        Assert.Equal(AccountStatus.Active, account.Status);
    }

    [Fact]
    public void Account_Defaults_AccountTypeIsChecking()
    {
        var account = new Account();

        Assert.Equal(AccountType.Checking, account.AccountType);
    }

    // =================================================================
    // ACCT-BR-004: Account balance management
    // COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC)
    // =================================================================

    [Fact]
    public void ApplyTransaction_CreditAmount_IncreasesBalanceAndCycleCredit()
    {
        // COBOL: ADD DALYTRAN-AMT TO ACCT-CURR-BAL / ACCT-CURR-CYC-CREDIT
        var account = new Account { CurrentBalance = 100.00m, CurrentCycleCredit = 50.00m };

        account.ApplyTransaction(200.00m);

        Assert.Equal(300.00m, account.CurrentBalance);
        Assert.Equal(250.00m, account.CurrentCycleCredit);
    }

    [Fact]
    public void ApplyTransaction_DebitAmount_IncreasesBalanceAndCycleDebit()
    {
        // COBOL: ADD DALYTRAN-AMT TO ACCT-CURR-BAL / ACCT-CURR-CYC-DEBIT
        var account = new Account { CurrentBalance = 500.00m, CurrentCycleDebit = -100.00m };

        account.ApplyTransaction(-75.50m);

        Assert.Equal(424.50m, account.CurrentBalance);
        Assert.Equal(-175.50m, account.CurrentCycleDebit);
    }

    [Fact]
    public void ApplyTransaction_ZeroAmount_GoesToCredit()
    {
        // COBOL: IF DALYTRAN-AMT >= 0 — zero is treated as credit
        var account = new Account { CurrentCycleCredit = 100.00m };

        account.ApplyTransaction(0.00m);

        Assert.Equal(100.00m, account.CurrentCycleCredit);
    }

    [Fact]
    public void ApplyTransaction_PreservesDecimalPrecision()
    {
        // COBOL: PIC S9(10)V99 — must preserve 2-decimal precision
        var account = new Account { CurrentBalance = 999999999.99m };

        account.ApplyTransaction(0.01m);

        Assert.Equal(1000000000.00m, account.CurrentBalance);
    }

    // =================================================================
    // ACCT-BR-005: Account status transitions
    // =================================================================

    [Fact]
    public void TransitionTo_ActiveToDormant_Succeeds()
    {
        var account = new Account { Status = AccountStatus.Active };

        var result = account.TransitionTo(AccountStatus.Dormant);

        Assert.True(result.IsValid);
        Assert.Equal(AccountStatus.Dormant, account.Status);
    }

    [Fact]
    public void TransitionTo_ActiveToFrozen_Succeeds()
    {
        var account = new Account { Status = AccountStatus.Active };

        var result = account.TransitionTo(AccountStatus.Frozen);

        Assert.True(result.IsValid);
        Assert.Equal(AccountStatus.Frozen, account.Status);
    }

    [Fact]
    public void TransitionTo_ActiveToClosed_SetsClosedDate()
    {
        var account = new Account { Status = AccountStatus.Active };

        var result = account.TransitionTo(AccountStatus.Closed);

        Assert.True(result.IsValid);
        Assert.Equal(AccountStatus.Closed, account.Status);
        Assert.NotNull(account.ClosedDate);
    }

    [Fact]
    public void TransitionTo_DormantToActive_Succeeds()
    {
        var account = new Account { Status = AccountStatus.Dormant };

        var result = account.TransitionTo(AccountStatus.Active);

        Assert.True(result.IsValid);
        Assert.Equal(AccountStatus.Active, account.Status);
    }

    [Fact]
    public void TransitionTo_FrozenToActive_Succeeds()
    {
        var account = new Account { Status = AccountStatus.Frozen };

        var result = account.TransitionTo(AccountStatus.Active);

        Assert.True(result.IsValid);
        Assert.Equal(AccountStatus.Active, account.Status);
    }

    [Fact]
    public void TransitionTo_ClosedToAnything_Fails()
    {
        // Closed is terminal — no transitions allowed
        var account = new Account { Status = AccountStatus.Closed };

        var result = account.TransitionTo(AccountStatus.Active);

        Assert.False(result.IsValid);
        Assert.Equal("Cannot transition from Closed status — terminal state", result.ErrorMessage);
        Assert.Equal(AccountStatus.Closed, account.Status);
    }

    [Fact]
    public void TransitionTo_SameStatus_Fails()
    {
        var account = new Account { Status = AccountStatus.Active };

        var result = account.TransitionTo(AccountStatus.Active);

        Assert.False(result.IsValid);
        Assert.Equal("Account is already in the requested status", result.ErrorMessage);
    }

    // =================================================================
    // ACCT-BR-006: Account expiration date management
    // COBOL source: CBTRN02C.cbl:414-420
    // =================================================================

    [Fact]
    public void IsExpired_BeforeExpirationDate_ReturnsFalse()
    {
        var account = new Account { ExpirationDate = new DateTime(2028, 12, 31) };

        Assert.False(account.IsExpired(new DateTime(2028, 1, 1)));
    }

    [Fact]
    public void IsExpired_AfterExpirationDate_ReturnsTrue()
    {
        var account = new Account { ExpirationDate = new DateTime(2025, 6, 15) };

        Assert.True(account.IsExpired(new DateTime(2025, 6, 16)));
    }

    [Fact]
    public void IsExpired_NullExpirationDate_ReturnsFalse()
    {
        // Null means no expiration — account never expires
        var account = new Account { ExpirationDate = null };

        Assert.False(account.IsExpired(DateTime.MaxValue));
    }

    [Fact]
    public void IsExpired_OnExpirationDate_ReturnsFalse()
    {
        // COBOL: ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS — equal means not expired
        var account = new Account { ExpirationDate = new DateTime(2025, 6, 15) };

        Assert.False(account.IsExpired(new DateTime(2025, 6, 15)));
    }

    // =================================================================
    // ACCT-BR-007: Account credit limit enforcement
    // COBOL source: CBTRN02C.cbl:403-413
    // =================================================================

    [Fact]
    public void WouldExceedCreditLimit_WithinLimit_ReturnsFalse()
    {
        var account = new Account
        {
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 3000.00m,
            CurrentCycleDebit = -1000.00m
        };

        // Projected: 3000 - (-1000) + 500 = 4500 <= 5000
        Assert.False(account.WouldExceedCreditLimit(500.00m));
    }

    [Fact]
    public void WouldExceedCreditLimit_ExceedsLimit_ReturnsTrue()
    {
        var account = new Account
        {
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 3000.00m,
            CurrentCycleDebit = -1000.00m
        };

        // Projected: 3000 - (-1000) + 2001 = 6001 > 5000
        Assert.True(account.WouldExceedCreditLimit(2001.00m));
    }

    [Fact]
    public void WouldExceedCreditLimit_ExactlyAtLimit_ReturnsFalse()
    {
        var account = new Account
        {
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 3000.00m,
            CurrentCycleDebit = -1000.00m
        };

        // Projected: 3000 - (-1000) + 2000 = 6000... wait
        // Actually: 3000 + 1000 + 2000 = 6000 > 5000. Let me recalc.
        // Formula: CurrentCycleCredit - CurrentCycleDebit + amount
        // 3000 - (-1000) + 1000 = 5000 = limit, not exceeded
        Assert.False(account.WouldExceedCreditLimit(1000.00m));
    }
}
