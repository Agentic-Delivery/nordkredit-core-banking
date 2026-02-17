using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.ParallelRun;

namespace NordKredit.UnitTests.ParallelRun;

/// <summary>
/// Unit tests for ParallelRunOrchestrator — coordinates parallel execution and comparison.
/// Validates traffic routing, output comparison, divergence tracking, and per-domain enablement.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class ParallelRunOrchestratorTests
{
    private readonly StubMainframeGateway _mainframeGateway = new();
    private readonly StubComparisonEngine _comparisonEngine = new();
    private readonly StubDivergenceStoreForOrchestrator _divergenceStore = new();

    // ===================================================================
    // AC: Framework routes production traffic to both systems simultaneously
    // When domain is enabled, both systems are called
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_DomainEnabled_CallsBothSystems()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        string azureResponse = /*lang=json,strict*/ """{"amount": 100.50}""";
        _mainframeGateway.Response = /*lang=json,strict*/ """{"amount": 100.50}""";
        _comparisonEngine.Result = CreateMatchResult();

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", azureResponse, "corr-001");

        Assert.True(result.BothSystemsResponded);
        Assert.Equal(/*lang=json,strict*/ """{"amount": 100.50}""", result.MainframeResponse);
        Assert.Equal(azureResponse, result.AzureResponse);
        Assert.NotNull(result.Comparison);
    }

    // ===================================================================
    // AC: Framework supports per-domain enablement
    // When domain is disabled, only Azure response is returned
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_DomainDisabled_ReturnsAzureOnlyWithNoComparison()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = false });
        var orchestrator = CreateOrchestrator(config);
        string azureResponse = /*lang=json,strict*/ """{"amount": 100.50}""";

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", azureResponse, "corr-002");

        Assert.False(result.BothSystemsResponded);
        Assert.Null(result.MainframeResponse);
        Assert.Equal(azureResponse, result.AzureResponse);
        Assert.Null(result.Comparison);
        Assert.False(_mainframeGateway.WasCalled);
    }

    // ===================================================================
    // AC: Domain not in configuration → disabled (mainframe-only)
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_DomainNotConfigured_ReturnsAzureOnly()
    {
        var config = CreateConfig(enabledDomains: new() { ["CardManagement"] = true });
        var orchestrator = CreateOrchestrator(config);
        string azureResponse = /*lang=json,strict*/ """{"amount": 100.50}""";

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", azureResponse, "corr-003");

        Assert.False(result.BothSystemsResponded);
        Assert.Null(result.Comparison);
    }

    // ===================================================================
    // AC: Outputs from both systems are compared automatically
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_BothRespond_ComparesOutputs()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.Response = /*lang=json,strict*/ """{"amount": 100.50}""";
        _comparisonEngine.Result = CreateMatchResult();

        await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", /*lang=json,strict*/ """{"amount": 100.50}""", "corr-004");

        Assert.True(_comparisonEngine.WasCalled);
        Assert.Equal("Transactions", _comparisonEngine.LastDomain);
        Assert.Equal("TransactionDetail", _comparisonEngine.LastOperation);
    }

    // ===================================================================
    // AC: Results are logged for audit trail (DORA compliance)
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_BothRespond_SavesDivergenceRecord()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.Response = /*lang=json,strict*/ """{"amount": 100.50}""";
        _comparisonEngine.Result = CreateMatchResult();

        await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", /*lang=json,strict*/ """{"amount": 100.50}""", "corr-005");

        Assert.Single(_divergenceStore.SavedRecords);
        var saved = _divergenceStore.SavedRecords[0];
        Assert.Equal("Transactions", saved.Domain);
        Assert.Equal("TransactionDetail", saved.Operation);
        Assert.Equal("corr-005", saved.CorrelationId);
        Assert.True(saved.IsMatch);
    }

    // ===================================================================
    // Mainframe timeout → returns Azure response, no comparison
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_MainframeTimesOut_ReturnsAzureResponseOnly()
    {
        var config = CreateConfig(
            enabledDomains: new() { ["Transactions"] = true },
            mainframeTimeout: TimeSpan.FromMilliseconds(1));
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.Delay = TimeSpan.FromSeconds(5);
        _mainframeGateway.Response = /*lang=json,strict*/ """{"amount": 100.50}""";

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", /*lang=json,strict*/ """{"amount": 100.50}""", "corr-006");

        Assert.True(result.MainframeTimedOut);
        Assert.Null(result.Comparison);
        Assert.Equal(/*lang=json,strict*/ """{"amount": 100.50}""", result.AzureResponse);
    }

    // ===================================================================
    // Mainframe error → captured in result, no comparison
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_MainframeError_ReturnsErrorWithAzureResponse()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.ThrowOnSend = new InvalidOperationException("Mainframe unavailable");

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", /*lang=json,strict*/ """{"amount": 100.50}""", "corr-007");

        Assert.False(result.BothSystemsResponded);
        Assert.NotNull(result.ErrorMessage);
        Assert.Contains("Mainframe unavailable", result.ErrorMessage);
        Assert.Null(result.Comparison);
    }

    // ===================================================================
    // Correlation ID passed through to all components
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_CorrelationId_PassedThrough()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.Response = "{}";
        _comparisonEngine.Result = CreateMatchResult();

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", "{}", "corr-008");

        Assert.Equal("corr-008", result.CorrelationId);
        Assert.Equal("corr-008", _mainframeGateway.LastCorrelationId);
    }

    // ===================================================================
    // Duration is tracked
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_TracksDuration()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.Response = "{}";
        _comparisonEngine.Result = CreateMatchResult();

        var result = await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", "{}", "corr-009");

        Assert.True(result.Duration >= TimeSpan.Zero);
    }

    // ===================================================================
    // Divergence record saved with mismatches
    // ===================================================================

    [Fact]
    public async Task ExecuteAsync_Divergence_SavesRecordWithMismatchDetails()
    {
        var config = CreateConfig(enabledDomains: new() { ["Transactions"] = true });
        var orchestrator = CreateOrchestrator(config);
        _mainframeGateway.Response = /*lang=json,strict*/ """{"amount": 100}""";
        _comparisonEngine.Result = CreateMismatchResult();

        await orchestrator.ExecuteAsync(
            "Transactions", "TransactionDetail", "{}", /*lang=json,strict*/ """{"amount": 200}""", "corr-010");

        Assert.Single(_divergenceStore.SavedRecords);
        Assert.False(_divergenceStore.SavedRecords[0].IsMatch);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private ParallelRunOrchestrator CreateOrchestrator(ParallelRunConfiguration config) =>
        new(
            _mainframeGateway,
            _comparisonEngine,
            _divergenceStore,
            config,
            NullLogger<ParallelRunOrchestrator>.Instance);

    private static ParallelRunConfiguration CreateConfig(
        Dictionary<string, bool>? enabledDomains = null,
        TimeSpan? mainframeTimeout = null) =>
        new()
        {
            EnabledDomains = enabledDomains ?? new() { ["Transactions"] = true },
            MainframeTimeout = mainframeTimeout ?? TimeSpan.FromSeconds(30)
        };

    private static ComparisonResult CreateMatchResult() =>
        new()
        {
            IsMatch = true,
            Domain = "Transactions",
            Operation = "TransactionDetail",
            Divergences = [],
            ComparedAt = DateTimeOffset.UtcNow,
            CorrelationId = "test"
        };

    private static ComparisonResult CreateMismatchResult() =>
        new()
        {
            IsMatch = false,
            Domain = "Transactions",
            Operation = "TransactionDetail",
            Divergences =
            [
                new FieldDivergence
                {
                    FieldName = "amount",
                    MainframeValue = "100",
                    AzureValue = "200",
                    Category = DivergenceCategory.DataMismatch
                }
            ],
            ComparedAt = DateTimeOffset.UtcNow,
            CorrelationId = "test"
        };
}

// ===================================================================
// Test doubles for ParallelRunOrchestrator
// ===================================================================

internal sealed class StubMainframeGateway : IMainframeGateway
{
    public string Response { get; set; } = "{}";
    public Exception? ThrowOnSend { get; set; }
    public TimeSpan Delay { get; set; } = TimeSpan.Zero;
    public bool WasCalled { get; private set; }
    public string? LastCorrelationId { get; private set; }

    public async Task<string> SendAsync(
        string domain, string operation, string requestPayload,
        string correlationId, CancellationToken cancellationToken)
    {
        WasCalled = true;
        LastCorrelationId = correlationId;

        if (ThrowOnSend is not null)
        {
            throw ThrowOnSend;
        }

        if (Delay > TimeSpan.Zero)
        {
            await Task.Delay(Delay, cancellationToken);
        }

        return Response;
    }
}

internal sealed class StubComparisonEngine : IComparisonEngine
{
    public ComparisonResult? Result { get; set; }
    public bool WasCalled { get; private set; }
    public string? LastDomain { get; private set; }
    public string? LastOperation { get; private set; }

    public ComparisonResult Compare(
        string domain, string operation,
        string mainframeResponse, string azureResponse, string correlationId)
    {
        WasCalled = true;
        LastDomain = domain;
        LastOperation = operation;
        return Result!;
    }
}

internal sealed class StubDivergenceStoreForOrchestrator : IDivergenceStore
{
    public List<DivergenceRecord> SavedRecords { get; } = [];

    public Task SaveAsync(DivergenceRecord record, CancellationToken cancellationToken = default)
    {
        SavedRecords.Add(record);
        return Task.CompletedTask;
    }

    public Task<IReadOnlyList<DivergenceRecord>> GetByDomainAsync(
        string domain, DateTimeOffset windowStart, DateTimeOffset windowEnd,
        CancellationToken cancellationToken = default) =>
        Task.FromResult<IReadOnlyList<DivergenceRecord>>([]);
}
