using System.Text.Json;

namespace NordKredit.ComparisonTests.Transactions;

/// <summary>
/// Comparison tests for transaction detail operation — parallel-run validation against mainframe output.
/// COBOL source: COTRN01C.cbl:85-296 (CICS transaction CT01 — transaction detail screen).
/// Business rules: TRN-BR-002 (transaction detail view), TRN-BR-007 (transaction data structures).
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention),
/// GDPR Art.15 (right of access).
///
/// Golden file: Transactions/GoldenFiles/transaction-detail-by-id.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Card number masking: Mainframe returns full card number in CICS screen.
///   Migrated system masks to last 4 digits per PCI-DSS compliance (intentional security improvement).
/// - Date format: Mainframe uses MM/DD/YY format.
///   Migrated system uses ISO 8601 (YYYY-MM-DDTHH:mm:ss). Both represent the same instant.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareTransactionDetail_ById_MatchesMainframeOutput
/// - CompareTransactionDetail_NotFound_MatchesMainframeOutput
/// </summary>
public class TransactionDetailComparisonTests
{
    private const string _goldenFilePath = "Transactions/GoldenFiles/transaction-detail-by-id.json";

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
    public void GoldenFile_ContainsAllDetailFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("transactionId", out _));
        Assert.True(root.TryGetProperty("cardNumber", out _));
        Assert.True(root.TryGetProperty("typeCode", out _));
        Assert.True(root.TryGetProperty("categoryCode", out _));
        Assert.True(root.TryGetProperty("source", out _));
        Assert.True(root.TryGetProperty("amount", out _));
        Assert.True(root.TryGetProperty("description", out _));
        Assert.True(root.TryGetProperty("originationTimestamp", out _));
        Assert.True(root.TryGetProperty("processingTimestamp", out _));
        Assert.True(root.TryGetProperty("merchantId", out _));
        Assert.True(root.TryGetProperty("merchantName", out _));
        Assert.True(root.TryGetProperty("merchantCity", out _));
        Assert.True(root.TryGetProperty("merchantZip", out _));
    }

    [Fact]
    public void GoldenFile_DocumentsKnownDifferences()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("_knownDifferences", out var differences));
        Assert.True(differences.TryGetProperty("cardNumberMasking", out _));
        Assert.True(differences.TryGetProperty("dateFormat", out _));
    }

    [Fact]
    public void GoldenFile_CardNumber_IsMaskedForPciDss()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var cardNumber = document.RootElement.GetProperty("cardNumber").GetString();

        Assert.NotNull(cardNumber);
        Assert.Matches(@"^\*{12}\d{4}$", cardNumber);
    }

    [Fact]
    public void GoldenFile_TransactionId_Is16Characters()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var id = document.RootElement.GetProperty("transactionId").GetString();

        Assert.NotNull(id);
        Assert.Equal(16, id.Length);
    }

    [Fact]
    public void GoldenFile_Amount_IsExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var amount = document.RootElement.GetProperty("amount").GetDecimal();

        Assert.Equal(1250.75m, amount);
    }

    [Fact]
    public void GoldenFile_TypeCode_Is2Characters()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var typeCode = document.RootElement.GetProperty("typeCode").GetString();

        Assert.NotNull(typeCode);
        Assert.Equal(2, typeCode.Length);
    }

    [Fact]
    public void GoldenFile_TimestampFormat_IsISO8601()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);

        var origTs = document.RootElement.GetProperty("originationTimestamp").GetString();
        var procTs = document.RootElement.GetProperty("processingTimestamp").GetString();

        Assert.NotNull(origTs);
        Assert.NotNull(procTs);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}$", origTs);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}$", procTs);
    }
}
