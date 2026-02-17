namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Comprehensive result of the lending batch pipeline.
/// Includes per-step results, timing, and SLA compliance.
/// Per ADR-001: plain C# orchestrator pattern with SLA monitoring.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// </summary>
public class LendingBatchResult
{
    public required bool Success { get; init; }
    public string? FailedStep { get; init; }
    public string? ErrorMessage { get; init; }
    public AmortizationProcessingResult? AmortizationResult { get; init; }
    public CollateralValuationResult? CollateralValuationResult { get; init; }
    public DelinquencyMonitoringResult? DelinquencyMonitoringResult { get; init; }
    public required DateTimeOffset StartedAt { get; init; }
    public required DateTimeOffset CompletedAt { get; init; }
    public TimeSpan Duration => CompletedAt - StartedAt;
    public required bool SlaBreached { get; init; }
}
