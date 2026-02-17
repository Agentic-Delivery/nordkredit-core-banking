namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Result of the amortization processing batch step.
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk).
/// </summary>
public class AmortizationProcessingResult
{
    /// <summary>Loans that were processed with their interest amounts.</summary>
    public required IReadOnlyList<ProcessedLoan> ProcessedLoans { get; init; }

    /// <summary>Total number of active loans evaluated.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of loans where interest was successfully applied.</summary>
    public required int SuccessCount { get; init; }

    /// <summary>Number of loans that were skipped (zero balance, expired, etc.).</summary>
    public required int SkippedCount { get; init; }

    /// <summary>Number of loans where processing failed.</summary>
    public required int FailedCount { get; init; }

    /// <summary>Total interest amount accrued across all loans.</summary>
    public required decimal TotalInterestAccrued { get; init; }

    public bool HasErrors => FailedCount > 0;

    /// <summary>
    /// A loan that was processed during amortization batch.
    /// </summary>
    public class ProcessedLoan
    {
        public required string AccountId { get; init; }
        public required decimal InterestAmount { get; init; }
        public required decimal CurrentBalance { get; init; }
        public required bool IsSuccess { get; init; }
        public string? FailureReason { get; init; }
    }
}
