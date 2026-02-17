namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Result of the nightly interest accrual batch step.
/// Business rule: DEP-BR-004 (interest calculation and accrual).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6.
/// </summary>
public class InterestAccrualResult
{
    /// <summary>Total number of active accounts processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of accounts that had interest accrued.</summary>
    public required int AccruedCount { get; init; }

    /// <summary>Number of accounts skipped (zero balance, missing product, etc.).</summary>
    public required int SkippedCount { get; init; }

    /// <summary>Number of accounts that failed during processing.</summary>
    public required int FailedCount { get; init; }

    /// <summary>Total interest accrued across all accounts (decimal precision).</summary>
    public required decimal TotalInterestAccrued { get; init; }
}
