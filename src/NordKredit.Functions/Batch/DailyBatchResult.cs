namespace NordKredit.Functions.Batch;

/// <summary>
/// Result of the daily batch pipeline orchestrator.
/// Replaces JCL job chain completion status for CBTRN01C → CBTRN02C → CBTRN03C.
/// Contains per-step results, timing information, and SLA compliance status.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// Business rules: TRN-BR-005 through TRN-BR-009 (pipeline integration).
/// </summary>
public class DailyBatchResult
{
    /// <summary>Whether all pipeline steps completed successfully.</summary>
    public required bool Success { get; init; }

    /// <summary>Name of the step that failed, or null if all succeeded.</summary>
    public string? FailedStep { get; init; }

    /// <summary>Error message from the failed step, or null if all succeeded.</summary>
    public string? ErrorMessage { get; init; }

    /// <summary>Result of step 1: card verification. Null if step was not reached.</summary>
    public CardVerificationResult? CardVerificationResult { get; init; }

    /// <summary>Result of step 2: credit/expiration validation. Null if step was not reached.</summary>
    public TransactionCreditValidationResult? CreditValidationResult { get; init; }

    /// <summary>Result of step 3: transaction posting. Null if step was not reached.</summary>
    public TransactionPostingResult? PostingResult { get; init; }

    /// <summary>Result of step 4: report generation. Null if step was not reached.</summary>
    public TransactionReportFunctionResult? ReportResult { get; init; }

    /// <summary>When the pipeline started (UTC).</summary>
    public required DateTimeOffset StartedAt { get; init; }

    /// <summary>When the pipeline finished (UTC).</summary>
    public required DateTimeOffset CompletedAt { get; init; }

    /// <summary>Total pipeline execution duration.</summary>
    public TimeSpan Duration => CompletedAt - StartedAt;

    /// <summary>Whether the pipeline completed after the 06:00 SLA deadline.</summary>
    public required bool SlaBreached { get; init; }
}
