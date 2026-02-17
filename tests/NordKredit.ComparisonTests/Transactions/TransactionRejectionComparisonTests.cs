using System.Text.Json;

namespace NordKredit.ComparisonTests.Transactions;

/// <summary>
/// Comparison tests for transaction rejection handling — parallel-run validation against mainframe output.
/// COBOL source: CBTRN02C.cbl:370-422 (credit limit and expiration validation, rejection recording).
/// Business rule: TRN-BR-005 (daily posting — rejection codes 100-103).
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
///
/// Golden file: Transactions/GoldenFiles/transaction-rejections.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Failure collection: Mainframe records only the last failure reason per transaction
///   (overwrite pattern — "last failure wins"). Migrated system collects ALL failure reasons
///   per transaction — intentional improvement for debugging and audit.
///
/// Reject codes (identical in both systems):
/// - 100: Invalid card number (not found in CARDXREF)
/// - 101: Account not found
/// - 102: Transaction exceeds credit limit (CycleCredit - CycleDebit + Amount > CreditLimit)
/// - 103: Account expired (OriginationDate > ExpirationDate)
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareRejections_SameTransactionsRejected_MatchesMainframeOutput
/// - CompareRejections_RejectCodes_MatchesMainframeOutput
/// - CompareRejections_AllFailuresCollected_DocumentedDifference
/// </summary>
public class TransactionRejectionComparisonTests
{
    private const string _goldenFilePath = "Transactions/GoldenFiles/transaction-rejections.json";

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
    public void GoldenFile_DocumentsKnownDifferences()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("_knownDifferences", out var differences));
        Assert.True(differences.TryGetProperty("failureCollection", out _));
    }

    [Fact]
    public void GoldenFile_ContainsRejections()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var rejections = document.RootElement.GetProperty("rejections");

        Assert.Equal(4, rejections.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_Rejections_HaveRequiredFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("rejections")[0];

        Assert.True(first.TryGetProperty("transactionId", out _));
        Assert.True(first.TryGetProperty("cardNumber", out _));
        Assert.True(first.TryGetProperty("accountId", out _));
        Assert.True(first.TryGetProperty("rejectCode", out _));
        Assert.True(first.TryGetProperty("rejectReason", out _));
        Assert.True(first.TryGetProperty("transactionAmount", out _));
    }

    [Fact]
    public void GoldenFile_RejectCodes_AreInValidRange()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var rejections = document.RootElement.GetProperty("rejections");

        foreach (var rejection in rejections.EnumerateArray())
        {
            var code = rejection.GetProperty("rejectCode").GetInt32();
            Assert.InRange(code, 100, 103);
        }
    }

    [Fact]
    public void GoldenFile_AllFourRejectCodes_AreCovered()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var rejections = document.RootElement.GetProperty("rejections");

        var codes = new HashSet<int>();
        foreach (var rejection in rejections.EnumerateArray())
        {
            codes.Add(rejection.GetProperty("rejectCode").GetInt32());
        }

        Assert.Contains(100, codes);
        Assert.Contains(101, codes);
        Assert.Contains(102, codes);
        Assert.Contains(103, codes);
    }

    [Fact]
    public void GoldenFile_Amounts_UseExactDecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var rejections = document.RootElement.GetProperty("rejections");

        foreach (var rejection in rejections.EnumerateArray())
        {
            var amount = rejection.GetProperty("transactionAmount").GetDecimal();
            Assert.Equal(amount, decimal.Round(amount, 2));
        }
    }

    [Fact]
    public void GoldenFile_TransactionIds_Are16Characters()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var rejections = document.RootElement.GetProperty("rejections");

        foreach (var rejection in rejections.EnumerateArray())
        {
            var id = rejection.GetProperty("transactionId").GetString();
            Assert.NotNull(id);
            Assert.Equal(16, id.Length);
        }
    }
}
