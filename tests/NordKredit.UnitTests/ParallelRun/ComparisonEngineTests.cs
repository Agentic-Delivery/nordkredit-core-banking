using NordKredit.Domain.ParallelRun;

namespace NordKredit.UnitTests.ParallelRun;

/// <summary>
/// Unit tests for ComparisonEngine — compares mainframe and Azure outputs.
/// Validates field-level comparison, known-difference filtering, and divergence categorization.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class ComparisonEngineTests
{
    private readonly ComparisonEngine _engine = new();

    // ===================================================================
    // AC: Outputs from both systems are captured and compared automatically
    // Identical JSON → match
    // ===================================================================

    [Fact]
    public void Compare_IdenticalJson_ReturnsMatch()
    {
        string json = /*lang=json,strict*/ """{"amount": 100.50, "status": "active"}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", json, json, "corr-001");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
        Assert.Equal("Transactions", result.Domain);
        Assert.Equal("TransactionDetail", result.Operation);
        Assert.Equal("corr-001", result.CorrelationId);
    }

    // ===================================================================
    // Different values → divergence with DataMismatch category
    // ===================================================================

    [Fact]
    public void Compare_DifferentValues_ReturnsDivergence()
    {
        string mainframe = /*lang=json,strict*/ """{"amount": 100.50, "status": "active"}""";
        string azure = /*lang=json,strict*/ """{"amount": 200.75, "status": "active"}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-002");

        Assert.False(result.IsMatch);
        Assert.Single(result.Divergences);
        Assert.Equal("amount", result.Divergences[0].FieldName);
        Assert.Equal("100.50", result.Divergences[0].MainframeValue);
        Assert.Equal("200.75", result.Divergences[0].AzureValue);
        Assert.Equal(DivergenceCategory.DataMismatch, result.Divergences[0].Category);
    }

    // ===================================================================
    // Missing field in Azure → MissingField divergence
    // ===================================================================

    [Fact]
    public void Compare_FieldMissingInAzure_ReturnsMissingFieldDivergence()
    {
        string mainframe = /*lang=json,strict*/ """{"amount": 100.50, "extra": "value"}""";
        string azure = /*lang=json,strict*/ """{"amount": 100.50}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-003");

        Assert.False(result.IsMatch);
        Assert.Single(result.Divergences);
        Assert.Equal("extra", result.Divergences[0].FieldName);
        Assert.Equal(DivergenceCategory.MissingField, result.Divergences[0].Category);
        Assert.Equal("value", result.Divergences[0].MainframeValue);
        Assert.Null(result.Divergences[0].AzureValue);
    }

    // ===================================================================
    // Missing field in mainframe → MissingField divergence
    // ===================================================================

    [Fact]
    public void Compare_FieldMissingInMainframe_ReturnsMissingFieldDivergence()
    {
        string mainframe = /*lang=json,strict*/ """{"amount": 100.50}""";
        string azure = /*lang=json,strict*/ """{"amount": 100.50, "extra": "value"}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-004");

        Assert.False(result.IsMatch);
        Assert.Single(result.Divergences);
        Assert.Equal("extra", result.Divergences[0].FieldName);
        Assert.Equal(DivergenceCategory.MissingField, result.Divergences[0].Category);
        Assert.Null(result.Divergences[0].MainframeValue);
        Assert.Equal("value", result.Divergences[0].AzureValue);
    }

    // ===================================================================
    // Known differences are filtered out — metadata fields starting with _
    // ===================================================================

    [Fact]
    public void Compare_MetadataFields_AreIgnored()
    {
        string mainframe = /*lang=json,strict*/ """{"amount": 100.50, "_comment": "mainframe version"}""";
        string azure = /*lang=json,strict*/ """{"amount": 100.50, "_comment": "azure version"}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-005");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
    }

    // ===================================================================
    // Known difference: card number masking (PCI-DSS)
    // Mainframe returns full card number, Azure masks to last 4 digits
    // ===================================================================

    [Fact]
    public void Compare_CardNumberMasking_IsKnownDifference()
    {
        string mainframe = /*lang=json,strict*/ """{"cardNumber": "4000000000001234", "amount": 100.50}""";
        string azure = /*lang=json,strict*/ """{"cardNumber": "************1234", "amount": 100.50}""";

        var result = _engine.Compare("CardManagement", "CardDetail", mainframe, azure, "corr-006");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
    }

    // ===================================================================
    // Known difference: date format (ISO 8601 vs MM/DD/YY)
    // ===================================================================

    [Fact]
    public void Compare_DateFormatDifference_IsKnownDifference()
    {
        string mainframe = /*lang=json,strict*/ """{"transactionDate": "01/15/26", "amount": 100.50}""";
        string azure = /*lang=json,strict*/ """{"transactionDate": "2026-01-15T00:00:00", "amount": 100.50}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-007");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
    }

    // ===================================================================
    // Multiple divergences in one comparison
    // ===================================================================

    [Fact]
    public void Compare_MultipleDifferences_ReturnsAllDivergences()
    {
        string mainframe = /*lang=json,strict*/ """{"amount": 100.50, "status": "A", "code": "01"}""";
        string azure = /*lang=json,strict*/ """{"amount": 200.75, "status": "B", "code": "01"}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-008");

        Assert.False(result.IsMatch);
        Assert.Equal(2, result.Divergences.Count);
    }

    // ===================================================================
    // Empty JSON objects → match
    // ===================================================================

    [Fact]
    public void Compare_EmptyObjects_ReturnsMatch()
    {
        var result = _engine.Compare("Transactions", "TransactionDetail", "{}", "{}", "corr-009");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
    }

    // ===================================================================
    // Nested objects are compared by their string representation
    // ===================================================================

    [Fact]
    public void Compare_NestedObjects_DifferentValues_ReturnsDivergence()
    {
        string mainframe = /*lang=json,strict*/ """{"merchant": {"name": "Store A"}, "amount": 100}""";
        string azure = /*lang=json,strict*/ """{"merchant": {"name": "Store B"}, "amount": 100}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-010");

        Assert.False(result.IsMatch);
        Assert.Single(result.Divergences);
        Assert.Equal("merchant", result.Divergences[0].FieldName);
    }

    // ===================================================================
    // _knownDifferences field is metadata and should be ignored
    // ===================================================================

    [Fact]
    public void Compare_KnownDifferencesMetadata_IsIgnored()
    {
        string mainframe = /*lang=json,strict*/ """{"amount": 100.50, "_knownDifferences": {"cardNumberMasking": "PCI-DSS"}}""";
        string azure = /*lang=json,strict*/ """{"amount": 100.50}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-011");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
    }

    // ===================================================================
    // Timestamp fields with known format difference
    // ===================================================================

    [Fact]
    public void Compare_TimestampFields_FormatDifference_IsKnownDifference()
    {
        string mainframe = /*lang=json,strict*/ """{"originationTimestamp": "01/15/26 10:30:00", "amount": 100}""";
        string azure = /*lang=json,strict*/ """{"originationTimestamp": "2026-01-15T10:30:00", "amount": 100}""";

        var result = _engine.Compare("Transactions", "TransactionDetail", mainframe, azure, "corr-012");

        Assert.True(result.IsMatch);
        Assert.Empty(result.Divergences);
    }

    // ===================================================================
    // Card masking with different last-4 digits → real divergence
    // ===================================================================

    [Fact]
    public void Compare_CardMasking_DifferentLast4_IsDivergence()
    {
        string mainframe = /*lang=json,strict*/ """{"cardNumber": "4000000000001234", "amount": 100.50}""";
        string azure = /*lang=json,strict*/ """{"cardNumber": "************5678", "amount": 100.50}""";

        var result = _engine.Compare("CardManagement", "CardDetail", mainframe, azure, "corr-013");

        Assert.False(result.IsMatch);
        Assert.Single(result.Divergences);
    }
}
