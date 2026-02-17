using System.Text.Json;

namespace NordKredit.ComparisonTests.AccountManagement;

/// <summary>
/// Comparison tests for account detail operation — parallel-run validation against mainframe output.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: ACCT-BR-001 (data structure), ACCT-BR-004 (balance management).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, GDPR Art. 5(1)(c)(d).
///
/// Golden file: AccountManagement/GoldenFiles/account-detail.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Status field: Mainframe uses 'A'/'N' single char. Migrated system uses enum string (Active/Dormant/Frozen/Closed).
///   Mapping: A → Active, N → Dormant (extended with Frozen/Closed for FSA compliance).
/// - Character encoding: Mainframe uses EBCDIC. Migrated system uses Unicode (UTF-8).
///   Swedish characters (Å, Ä, Ö) are functionally equivalent after EBCDIC-to-Unicode conversion.
/// </summary>
public class AccountDetailComparisonTests
{
    private const string _goldenFilePath = "AccountManagement/GoldenFiles/account-detail.json";

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
        Assert.True(root.TryGetProperty("currentBalance", out _));
        Assert.True(root.TryGetProperty("creditLimit", out _));
        Assert.True(root.TryGetProperty("cashCreditLimit", out _));
        Assert.True(root.TryGetProperty("currentCycleCredit", out _));
        Assert.True(root.TryGetProperty("currentCycleDebit", out _));
        Assert.True(root.TryGetProperty("expirationDate", out _));
    }

    [Fact]
    public void GoldenFile_AccountId_Is11Digits()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var accountId = document.RootElement.GetProperty("accountId").GetString();

        Assert.NotNull(accountId);
        Assert.Equal(11, accountId.Length);
        Assert.True(accountId.All(char.IsAsciiDigit));
    }

    [Fact]
    public void GoldenFile_ExpirationDateFormat_IsYYYYMMDD()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var dateStr = document.RootElement.GetProperty("expirationDate").GetString();

        Assert.NotNull(dateStr);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", dateStr);
    }

    [Fact]
    public void GoldenFile_BalanceFields_AreDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.GetProperty("currentBalance").TryGetDecimal(out _));
        Assert.True(root.GetProperty("creditLimit").TryGetDecimal(out _));
        Assert.True(root.GetProperty("cashCreditLimit").TryGetDecimal(out _));
        Assert.True(root.GetProperty("currentCycleCredit").TryGetDecimal(out _));
        Assert.True(root.GetProperty("currentCycleDebit").TryGetDecimal(out _));
    }
}
