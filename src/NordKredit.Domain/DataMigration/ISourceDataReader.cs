namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Reads changed records from the Db2 source system for incremental migration.
/// Infrastructure implements with Db2 connectivity.
/// COBOL source: All VSAM file structures (ACCTFILE, CARDDAT, CXREF, TRANSACT, etc.).
/// Regulation: GDPR Art. 5(1)(d) — accuracy of personal data during migration.
/// </summary>
public interface ISourceDataReader
{
    /// <summary>
    /// Reads records from the source table that have changed since the given timestamp.
    /// Returns records ordered by source timestamp ascending.
    /// </summary>
    /// <param name="tableName">The source table name.</param>
    /// <param name="sinceTimestamp">Only return records changed after this timestamp.</param>
    /// <param name="batchSize">Maximum number of records to return.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    Task<IReadOnlyList<SourceRecord>> ReadChangedRecordsAsync(
        string tableName,
        DateTimeOffset sinceTimestamp,
        int batchSize,
        CancellationToken cancellationToken = default);
}
