namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Checks referential integrity by verifying that foreign key values exist in referenced tables.
/// Infrastructure implements with Azure SQL queries.
/// </summary>
public interface IReferentialIntegrityChecker
{
    /// <summary>
    /// Finds which of the given key values do NOT exist in the referenced table/column.
    /// Returns the set of missing keys.
    /// </summary>
    Task<IReadOnlyList<string>> FindMissingKeysAsync(
        string tableName,
        string columnName,
        IReadOnlyCollection<string> keyValues,
        CancellationToken cancellationToken = default);
}
