namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Result of a single migration batch execution.
/// Contains counts, timing, and validation results.
/// </summary>
public class MigrationBatchResult
{
    /// <summary>Correlation ID for this batch.</summary>
    public required string BatchCorrelationId { get; init; }

    /// <summary>Table that was synced.</summary>
    public required string TableName { get; init; }

    /// <summary>Number of records read from source.</summary>
    public required int RecordsRead { get; init; }

    /// <summary>Number of records successfully written to target.</summary>
    public required int RecordsWritten { get; init; }

    /// <summary>Number of records that failed conversion or write.</summary>
    public required int RecordsFailed { get; init; }

    /// <summary>Whether post-sync validation passed.</summary>
    public required bool ValidationPassed { get; init; }

    /// <summary>Validation errors, if any.</summary>
    public IReadOnlyList<string> ValidationErrors { get; init; } = [];

    /// <summary>Duration of the batch execution.</summary>
    public required TimeSpan Duration { get; init; }

    /// <summary>Whether the batch completed successfully (all records written, validation passed).</summary>
    public bool IsSuccess => RecordsFailed == 0 && ValidationPassed;
}
