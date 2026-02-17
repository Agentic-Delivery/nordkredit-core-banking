namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Result of the delinquency monitoring batch step.
/// Business rule: LND-BR-008 (delinquency management).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Inkassolagen 1974:182,
///              AML/KYC (delinquent accounts feed AML screening).
/// </summary>
public class DelinquencyMonitoringResult
{
    /// <summary>Total number of loans evaluated for delinquency.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of loans newly transitioned to Delinquent status.</summary>
    public required int NewlyDelinquentCount { get; init; }

    /// <summary>Number of loans flagged for AML screening.</summary>
    public required int FlaggedForAmlScreeningCount { get; init; }

    /// <summary>Number of loans with high LTV flagged as elevated risk.</summary>
    public required int ElevatedRiskCount { get; init; }

    public bool HasAlerts => NewlyDelinquentCount > 0 || FlaggedForAmlScreeningCount > 0;
}
