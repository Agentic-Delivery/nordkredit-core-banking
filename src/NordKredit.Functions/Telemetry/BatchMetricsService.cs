using System.Diagnostics.Metrics;

namespace NordKredit.Functions.Telemetry;

/// <summary>
/// Custom metrics for batch SLA monitoring.
/// Tracks pipeline duration, posted transaction count, and rejected transaction count.
/// DORA Art.11: ICT risk monitoring — batch SLA compliance metrics.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public sealed class BatchMetricsService : IDisposable
{
    public const string MeterName = "NordKredit.Batch";

    private readonly Meter _meter;

    public BatchMetricsService(IMeterFactory meterFactory)
    {
        _meter = meterFactory.Create(MeterName);
        BatchDuration = _meter.CreateHistogram<double>(
            "batch.duration",
            unit: "s",
            description: "Duration of the daily batch pipeline in seconds");
        TransactionsPosted = _meter.CreateCounter<long>(
            "batch.transactions.posted",
            unit: "{transactions}",
            description: "Number of transactions successfully posted");
        TransactionsRejected = _meter.CreateCounter<long>(
            "batch.transactions.rejected",
            unit: "{transactions}",
            description: "Number of transactions rejected during validation");
    }

    public Histogram<double> BatchDuration { get; }
    public Counter<long> TransactionsPosted { get; }
    public Counter<long> TransactionsRejected { get; }

    public void Dispose() => _meter.Dispose();
}
