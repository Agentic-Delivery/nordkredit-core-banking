using System.Text.Json;
using NordKredit.Domain.AccountManagement;

namespace NordKredit.ComparisonTests.AccountManagement;

/// <summary>
/// Comparison tests for account balance update — parallel-run validation against mainframe output.
/// COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
/// Business rule: ACCT-BR-004 (balance management).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 7, PSD2 Art. 64, DORA Art. 11.
///
/// Golden file: AccountManagement/GoldenFiles/account-balance-update.json
/// Contains expected mainframe output for a credit transaction balance update.
///
/// Validates that .NET ApplyTransaction produces identical results to COBOL
/// 2800-UPDATE-ACCOUNT-REC paragraph for the three-field balance tracking:
///   ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT.
/// </summary>
public class AccountBalanceComparisonTests
{
    private const string _goldenFilePath = "AccountManagement/GoldenFiles/account-balance-update.json";

    [Fact]
    public void GoldenFile_Exists() =>
        Assert.True(File.Exists(_goldenFilePath), $"Golden file not found: {_goldenFilePath}");

    [Fact]
    public void GoldenFile_IsValidJson()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.NotNull(document);
    }

    [Fact]
    public void ApplyTransaction_MatchesMainframeOutput()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var transactionAmount = root.GetProperty("transactionAmount").GetDecimal();
        var previousBalance = root.GetProperty("previousBalance").GetDecimal();
        var expectedBalance = root.GetProperty("expectedBalance").GetDecimal();
        var previousCycleCredit = root.GetProperty("previousCycleCredit").GetDecimal();
        var expectedCycleCredit = root.GetProperty("expectedCycleCredit").GetDecimal();
        var previousCycleDebit = root.GetProperty("previousCycleDebit").GetDecimal();
        var expectedCycleDebit = root.GetProperty("expectedCycleDebit").GetDecimal();

        var account = new Account
        {
            Id = root.GetProperty("accountId").GetString()!,
            CurrentBalance = previousBalance,
            CurrentCycleCredit = previousCycleCredit,
            CurrentCycleDebit = previousCycleDebit
        };

        account.ApplyTransaction(transactionAmount);

        Assert.Equal(expectedBalance, account.CurrentBalance);
        Assert.Equal(expectedCycleCredit, account.CurrentCycleCredit);
        Assert.Equal(expectedCycleDebit, account.CurrentCycleDebit);
    }
}
