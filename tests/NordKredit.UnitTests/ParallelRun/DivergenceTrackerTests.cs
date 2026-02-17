using NordKredit.Domain.ParallelRun;

namespace NordKredit.UnitTests.ParallelRun;

/// <summary>
/// Unit tests for DivergenceTracker — aggregates metrics and checks thresholds.
/// Validates match rate calculation, category breakdown, affected accounts, and alerting.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class DivergenceTrackerTests
{
    private readonly StubDivergenceStore _store = new();

    // ===================================================================
    // AC: Divergence is tracked with metrics (match rate)
    // ===================================================================

    [Fact]
    public async Task GetMetrics_AllMatches_Returns100PercentMatchRate()
    {
        var config = CreateConfig(threshold: 0.01);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records =
        [
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: true)
        ];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.Equal(3, metrics.TotalComparisons);
        Assert.Equal(3, metrics.MatchCount);
        Assert.Equal(0, metrics.DivergenceCount);
        Assert.Equal(1.0, metrics.MatchRate);
        Assert.Equal(0.0, metrics.DivergenceRate);
        Assert.False(metrics.ThresholdExceeded);
    }

    // ===================================================================
    // AC: Divergence is tracked with metrics (failure categories)
    // ===================================================================

    [Fact]
    public async Task GetMetrics_MixedResults_CalculatesCorrectRates()
    {
        var config = CreateConfig(threshold: 0.05);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records =
        [
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: false, category: DivergenceCategory.DataMismatch),
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: false, category: DivergenceCategory.MissingField)
        ];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.Equal(5, metrics.TotalComparisons);
        Assert.Equal(3, metrics.MatchCount);
        Assert.Equal(2, metrics.DivergenceCount);
        Assert.Equal(0.6, metrics.MatchRate, 2);
        Assert.Equal(0.4, metrics.DivergenceRate, 2);
        Assert.True(metrics.ThresholdExceeded);
    }

    // ===================================================================
    // AC: Divergence tracked with failure categories
    // ===================================================================

    [Fact]
    public async Task GetMetrics_ReturnsCategoryBreakdown()
    {
        var config = CreateConfig(threshold: 0.5);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records =
        [
            CreateRecord("Transactions", isMatch: false, category: DivergenceCategory.DataMismatch),
            CreateRecord("Transactions", isMatch: false, category: DivergenceCategory.DataMismatch),
            CreateRecord("Transactions", isMatch: false, category: DivergenceCategory.MissingField),
            CreateRecord("Transactions", isMatch: false, category: DivergenceCategory.EncodingDifference)
        ];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.Equal(2, metrics.CategoryBreakdown[DivergenceCategory.DataMismatch]);
        Assert.Equal(1, metrics.CategoryBreakdown[DivergenceCategory.MissingField]);
        Assert.Equal(1, metrics.CategoryBreakdown[DivergenceCategory.EncodingDifference]);
    }

    // ===================================================================
    // AC: Divergence tracked with affected accounts
    // ===================================================================

    [Fact]
    public async Task GetMetrics_ReturnsAffectedAccounts()
    {
        var config = CreateConfig(threshold: 0.5);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records =
        [
            CreateRecord("Transactions", isMatch: false, accountId: "00000000001"),
            CreateRecord("Transactions", isMatch: false, accountId: "00000000002"),
            CreateRecord("Transactions", isMatch: false, accountId: "00000000001"), // duplicate
            CreateRecord("Transactions", isMatch: true, accountId: "00000000003")
        ];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.Equal(2, metrics.AffectedAccounts.Count);
        Assert.Contains("00000000001", metrics.AffectedAccounts);
        Assert.Contains("00000000002", metrics.AffectedAccounts);
    }

    // ===================================================================
    // AC: Alerting mechanism flags divergence above configurable thresholds
    // ===================================================================

    [Fact]
    public async Task GetMetrics_DivergenceBelowThreshold_ThresholdNotExceeded()
    {
        var config = CreateConfig(threshold: 0.5);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records =
        [
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: false) // 25% divergence < 50% threshold
        ];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.False(metrics.ThresholdExceeded);
    }

    [Fact]
    public async Task GetMetrics_DivergenceAboveThreshold_ThresholdExceeded()
    {
        var config = CreateConfig(threshold: 0.1);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records =
        [
            CreateRecord("Transactions", isMatch: true),
            CreateRecord("Transactions", isMatch: false) // 50% divergence > 10% threshold
        ];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.True(metrics.ThresholdExceeded);
    }

    // ===================================================================
    // No comparisons → 100% match rate, threshold not exceeded
    // ===================================================================

    [Fact]
    public async Task GetMetrics_NoRecords_ReturnsEmptyMetrics()
    {
        var config = CreateConfig(threshold: 0.01);
        var tracker = new DivergenceTracker(_store, config);
        var now = DateTimeOffset.UtcNow;

        _store.Records = [];

        var metrics = await tracker.GetMetricsAsync("Transactions", now.AddHours(-1), now);

        Assert.Equal(0, metrics.TotalComparisons);
        Assert.Equal(1.0, metrics.MatchRate);
        Assert.Equal(0.0, metrics.DivergenceRate);
        Assert.False(metrics.ThresholdExceeded);
    }

    // ===================================================================
    // Metrics include time window
    // ===================================================================

    [Fact]
    public async Task GetMetrics_IncludesTimeWindow()
    {
        var config = CreateConfig(threshold: 0.01);
        var tracker = new DivergenceTracker(_store, config);
        var from = new DateTimeOffset(2026, 1, 16, 0, 0, 0, TimeSpan.Zero);
        var to = new DateTimeOffset(2026, 1, 16, 23, 59, 59, TimeSpan.Zero);

        _store.Records = [];

        var metrics = await tracker.GetMetricsAsync("Transactions", from, to);

        Assert.Equal("Transactions", metrics.Domain);
        Assert.Equal(from, metrics.WindowStart);
        Assert.Equal(to, metrics.WindowEnd);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static ParallelRunConfiguration CreateConfig(double threshold) =>
        new() { DivergenceThreshold = threshold };

    private static DivergenceRecord CreateRecord(
        string domain,
        bool isMatch,
        DivergenceCategory category = DivergenceCategory.DataMismatch,
        string? accountId = null) =>
        new()
        {
            Id = Guid.NewGuid().ToString(),
            Domain = domain,
            Operation = "TransactionDetail",
            CorrelationId = Guid.NewGuid().ToString(),
            IsMatch = isMatch,
            MainframeResponse = "{}",
            AzureResponse = "{}",
            DivergencesJson = isMatch ? null : $$$"""[{"category": "{{{category}}}"}]""",
            AccountId = isMatch ? null : (accountId ?? "00000000001"),
            RecordedAt = DateTimeOffset.UtcNow
        };
}

/// <summary>
/// Stub implementation of IDivergenceStore for unit testing.
/// </summary>
internal sealed class StubDivergenceStore : IDivergenceStore
{
    public List<DivergenceRecord> Records { get; set; } = [];
    public List<DivergenceRecord> SavedRecords { get; } = [];

    public Task SaveAsync(DivergenceRecord record, CancellationToken cancellationToken = default)
    {
        SavedRecords.Add(record);
        return Task.CompletedTask;
    }

    public Task<IReadOnlyList<DivergenceRecord>> GetByDomainAsync(
        string domain,
        DateTimeOffset windowStart,
        DateTimeOffset windowEnd,
        CancellationToken cancellationToken = default)
    {
        IReadOnlyList<DivergenceRecord> filtered =
            [.. Records.Where(r => r.Domain == domain)];
        return Task.FromResult(filtered);
    }
}
