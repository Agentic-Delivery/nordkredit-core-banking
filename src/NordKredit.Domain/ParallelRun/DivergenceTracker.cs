using System.Text.Json;

namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Aggregates divergence metrics and checks configurable thresholds for alerting.
/// Provides match rate, failure category breakdown, and affected account tracking.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class DivergenceTracker
{
    private readonly IDivergenceStore _store;
    private readonly ParallelRunConfiguration _config;

    public DivergenceTracker(IDivergenceStore store, ParallelRunConfiguration config)
    {
        _store = store;
        _config = config;
    }

    /// <summary>
    /// Calculates divergence metrics for a domain within a time window.
    /// Includes match rate, category breakdown, affected accounts, and threshold check.
    /// </summary>
    public async Task<DivergenceMetrics> GetMetricsAsync(
        string domain,
        DateTimeOffset windowStart,
        DateTimeOffset windowEnd,
        CancellationToken cancellationToken = default)
    {
        var records = await _store.GetByDomainAsync(
            domain, windowStart, windowEnd, cancellationToken);

        int totalComparisons = records.Count;
        int matchCount = records.Count(r => r.IsMatch);
        int divergenceCount = totalComparisons - matchCount;

        double divergenceRate = totalComparisons == 0 ? 0.0 : (double)divergenceCount / totalComparisons;
        var thresholdExceeded = divergenceRate > _config.DivergenceThreshold;

        var categoryBreakdown = BuildCategoryBreakdown(records);

        IReadOnlyList<string> affectedAccounts = [.. records
            .Where(r => !r.IsMatch && r.AccountId is not null)
            .Select(r => r.AccountId!)
            .Distinct()];

        return new DivergenceMetrics
        {
            Domain = domain,
            TotalComparisons = totalComparisons,
            MatchCount = matchCount,
            DivergenceCount = divergenceCount,
            ThresholdExceeded = thresholdExceeded,
            CategoryBreakdown = categoryBreakdown,
            AffectedAccounts = affectedAccounts,
            WindowStart = windowStart,
            WindowEnd = windowEnd
        };
    }

    private static Dictionary<DivergenceCategory, int> BuildCategoryBreakdown(
        IReadOnlyList<DivergenceRecord> records)
    {
        Dictionary<DivergenceCategory, int> breakdown = [];

        foreach (var record in records)
        {
            if (record.IsMatch || record.DivergencesJson is null)
            {
                continue;
            }

            var category = ParseCategory(record.DivergencesJson);
            breakdown[category] = breakdown.TryGetValue(category, out int count)
                ? count + 1
                : 1;
        }

        return breakdown;
    }

    private static DivergenceCategory ParseCategory(string divergencesJson)
    {
        try
        {
            using var doc = JsonDocument.Parse(divergencesJson);
            var root = doc.RootElement;
            if (root.ValueKind == JsonValueKind.Array && root.GetArrayLength() > 0)
            {
                var first = root[0];
                if (first.TryGetProperty("category", out var categoryElement))
                {
                    var categoryStr = categoryElement.GetString();
                    if (categoryStr is not null && Enum.TryParse<DivergenceCategory>(categoryStr, out var parsed))
                    {
                        return parsed;
                    }
                }
            }
        }
        catch (JsonException)
        {
            // Fall through to default
        }

        return DivergenceCategory.DataMismatch;
    }
}
