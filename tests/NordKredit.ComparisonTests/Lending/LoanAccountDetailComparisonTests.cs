using System.Text.Json;

namespace NordKredit.ComparisonTests.Lending;

/// <summary>
/// Comparison tests for loan account detail inquiry — parallel-run validation against mainframe output.
/// COBOL source: CVACT01Y.cpy — ACCOUNT-RECORD (300 bytes), primary key lookup on ACCT-ID.
/// Business rules: LND-BR-001 (data structure), LND-BR-002 (credit limit enforcement).
/// Regulations: FSA FFFS 2014:5 Ch.6, Consumer Credit Directive Art.10, GDPR Art.15.
///
/// Golden file: Lending/GoldenFiles/loan-account-detail.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Concurrency: COBOL uses field-by-field comparison. Migrated system uses SQL rowversion (ETag).
/// - Status values: COBOL uses 'Y'/'N'. Migrated system uses Active/Delinquent/Defaulted/Frozen/PaidOff/Closed.
/// - Available credit: Derived field not in COBOL. Migrated system calculates CreditLimit - (CycleCredit - CycleDebit).
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareLoanAccountDetail_Active_MatchesMainframeOutput
/// - CompareLoanAccountDetail_NotFound_MatchesMainframeOutput
/// - CompareLoanAccountDetail_Expired_MatchesMainframeOutput
/// </summary>
public class LoanAccountDetailComparisonTests
{
    private const string _goldenFilePath = "Lending/GoldenFiles/loan-account-detail.json";

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
    public void GoldenFile_ContainsExpectedFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("accountId", out _));
        Assert.True(root.TryGetProperty("status", out _));
        Assert.True(root.TryGetProperty("loanType", out _));
        Assert.True(root.TryGetProperty("currentBalance", out _));
        Assert.True(root.TryGetProperty("creditLimit", out _));
        Assert.True(root.TryGetProperty("cashCreditLimit", out _));
        Assert.True(root.TryGetProperty("availableCredit", out _));
        Assert.True(root.TryGetProperty("currentCycleCredit", out _));
        Assert.True(root.TryGetProperty("currentCycleDebit", out _));
        Assert.True(root.TryGetProperty("disclosureGroupId", out _));
        Assert.True(root.TryGetProperty("borrowerName", out _));
        Assert.True(root.TryGetProperty("originationDate", out _));
    }

    [Fact]
    public void GoldenFile_AccountId_Is11Digits()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var id = document.RootElement.GetProperty("accountId").GetString();

        Assert.NotNull(id);
        Assert.Equal(11, id.Length);
        Assert.Matches(@"^\d{11}$", id);
    }

    [Fact]
    public void GoldenFile_DisclosureGroupId_IsWithinMaxLength()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var groupId = document.RootElement.GetProperty("disclosureGroupId").GetString();

        Assert.NotNull(groupId);
        Assert.True(groupId.Length <= 10, "Disclosure group ID exceeds COBOL PIC X(10) limit");
    }

    [Fact]
    public void GoldenFile_DateFormat_IsISO8601()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var originationDate = root.GetProperty("originationDate").GetString();
        Assert.NotNull(originationDate);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", originationDate);

        var expirationDate = root.GetProperty("expirationDate").GetString();
        Assert.NotNull(expirationDate);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", expirationDate);
    }

    [Fact]
    public void GoldenFile_Balance_IsExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var balance = root.GetProperty("currentBalance").GetDecimal();
        Assert.Equal(balance, decimal.Round(balance, 2));

        var creditLimit = root.GetProperty("creditLimit").GetDecimal();
        Assert.Equal(creditLimit, decimal.Round(creditLimit, 2));

        var cycleCredit = root.GetProperty("currentCycleCredit").GetDecimal();
        Assert.Equal(cycleCredit, decimal.Round(cycleCredit, 2));

        var cycleDebit = root.GetProperty("currentCycleDebit").GetDecimal();
        Assert.Equal(cycleDebit, decimal.Round(cycleDebit, 2));
    }

    [Fact]
    public void GoldenFile_AvailableCredit_MatchesCalculation()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var creditLimit = root.GetProperty("creditLimit").GetDecimal();
        var cycleCredit = root.GetProperty("currentCycleCredit").GetDecimal();
        var cycleDebit = root.GetProperty("currentCycleDebit").GetDecimal();
        var availableCredit = root.GetProperty("availableCredit").GetDecimal();

        var expected = creditLimit - (cycleCredit - cycleDebit);
        Assert.Equal(expected, availableCredit);
    }
}
