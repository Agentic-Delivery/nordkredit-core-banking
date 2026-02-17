namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Persists parallel-run comparison results for DORA audit trail.
/// Infrastructure layer implements with Azure SQL or Blob Storage.
/// Regulations: DORA Art.11 (ICT system testing — audit trail requirement).
/// </summary>
public interface IDivergenceStore
{
    /// <summary>
    /// Saves a divergence record for audit trail.
    /// </summary>
    Task SaveAsync(DivergenceRecord record, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets divergence records for a domain within a time window.
    /// Used for metrics aggregation and threshold checking.
    /// </summary>
    Task<IReadOnlyList<DivergenceRecord>> GetByDomainAsync(
        string domain,
        DateTimeOffset windowStart,
        DateTimeOffset windowEnd,
        CancellationToken cancellationToken = default);
}
