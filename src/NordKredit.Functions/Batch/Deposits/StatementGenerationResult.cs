namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Result of the monthly statement generation batch step.
/// Business rule: DEP-BR-004 (interest posting), DEP-BR-001 (account data).
/// Regulations: FSA FFFS 2014:5 Ch. 7 (financial reporting), PSD2 Art. 57.
/// </summary>
public class StatementGenerationResult
{
    /// <summary>Total number of active accounts processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of statements generated successfully.</summary>
    public required int StatementsGenerated { get; init; }

    /// <summary>Number of accounts skipped (no activity, etc.).</summary>
    public required int SkippedCount { get; init; }

    /// <summary>Number of accounts that had interest posted to balance.</summary>
    public required int InterestPostedCount { get; init; }

    /// <summary>Total interest posted across all accounts.</summary>
    public required decimal TotalInterestPosted { get; init; }
}
