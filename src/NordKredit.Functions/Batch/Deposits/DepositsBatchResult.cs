namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Result of the deposits batch pipeline orchestrator.
/// Replaces JCL job chain for deposit interest and statement processing.
/// Contains per-step results, timing information, and SLA compliance status.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// Business rules: DEP-BR-004 (interest accrual), DEP-BR-001 (account data).
/// </summary>
public class DepositsBatchResult
{
    /// <summary>Whether all pipeline steps completed successfully.</summary>
    public required bool Success { get; init; }

    /// <summary>Name of the step that failed, or null if all succeeded.</summary>
    public string? FailedStep { get; init; }

    /// <summary>Error message from the failed step, or null if all succeeded.</summary>
    public string? ErrorMessage { get; init; }

    /// <summary>Result of step 1: interest accrual. Null if step was not reached.</summary>
    public InterestAccrualResult? InterestAccrualResult { get; init; }

    /// <summary>Result of step 2: statement generation. Null if step was not reached.</summary>
    public StatementGenerationResult? StatementGenerationResult { get; init; }

    /// <summary>When the pipeline started (UTC).</summary>
    public required DateTimeOffset StartedAt { get; init; }

    /// <summary>When the pipeline finished (UTC).</summary>
    public required DateTimeOffset CompletedAt { get; init; }

    /// <summary>Total pipeline execution duration.</summary>
    public TimeSpan Duration => CompletedAt - StartedAt;

    /// <summary>Whether the pipeline completed after the 06:00 SLA deadline.</summary>
    public required bool SlaBreached { get; init; }
}
