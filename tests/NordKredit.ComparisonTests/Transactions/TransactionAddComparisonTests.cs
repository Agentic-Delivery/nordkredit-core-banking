using System.Text.Json;

namespace NordKredit.ComparisonTests.Transactions;

/// <summary>
/// Comparison tests for transaction add operation — parallel-run validation against mainframe output.
/// COBOL source: COTRN02C.cbl:442-749 (validate → generate ID → WRITE TRANFILE).
/// Business rule: TRN-BR-003 (transaction add with validation).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), PSD2 Art.64 (transaction data).
///
/// Golden file: Transactions/GoldenFiles/transaction-add-success.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Error collection: Mainframe validates sequentially and exits on first error.
///   Migrated system uses same sequential pattern — no difference for add validation.
/// - ID generation: Both use sequential 16-digit IDs. Migrated system uses same format.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareTransactionAdd_Success_MatchesMainframeOutput
/// - CompareTransactionAdd_ValidationFailure_MatchesMainframeOutput
/// - CompareTransactionAdd_DuplicateKey_MatchesMainframeOutput
/// </summary>
public class TransactionAddComparisonTests
{
    private const string _goldenFilePath = "Transactions/GoldenFiles/transaction-add-success.json";

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
    public void GoldenFile_ContainsExpectedResultFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("isSuccess", out _));
        Assert.True(root.TryGetProperty("confirmationRequired", out _));
        Assert.True(root.TryGetProperty("transactionId", out _));
        Assert.True(root.TryGetProperty("message", out _));
    }

    [Fact]
    public void GoldenFile_SuccessResult_HasTransactionId()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.GetProperty("isSuccess").GetBoolean());
        Assert.False(root.GetProperty("confirmationRequired").GetBoolean());

        var transactionId = root.GetProperty("transactionId").GetString();
        Assert.NotNull(transactionId);
        Assert.Equal(16, transactionId.Length);
    }

    [Fact]
    public void GoldenFile_SuccessMessage_MatchesCobolFormat()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var message = document.RootElement.GetProperty("message").GetString();

        Assert.NotNull(message);
        Assert.Matches(@"^Transaction added successfully\.\s+Your Tran ID is \d{16}\.$", message);
    }
}
