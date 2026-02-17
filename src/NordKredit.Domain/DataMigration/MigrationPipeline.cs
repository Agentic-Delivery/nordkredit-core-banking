using System.Diagnostics;
using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Orchestrates incremental data migration from Db2 to Azure SQL.
/// Reads changed records, applies EBCDIC-to-Unicode conversion, writes to target,
/// validates referential integrity, and maintains audit trail.
/// COBOL source: All copybook structures during Db2 → Azure SQL migration.
/// Regulations: GDPR Art. 5(1)(d) (accuracy), GDPR Art.17 (erasure), DORA Art.11 (audit).
/// </summary>
public partial class MigrationPipeline
{
    private readonly ISourceDataReader _sourceReader;
    private readonly ITargetDataWriter _targetWriter;
    private readonly IMigrationAuditLog _auditLog;
    private readonly IMigrationStateStore _stateStore;
    private readonly FieldConverter _fieldConverter;
    private readonly ReferentialIntegrityValidator _integrityValidator;
    private readonly MigrationPipelineConfiguration _config;
    private readonly ILogger<MigrationPipeline> _logger;

    public MigrationPipeline(
        ISourceDataReader sourceReader,
        ITargetDataWriter targetWriter,
        IMigrationAuditLog auditLog,
        IMigrationStateStore stateStore,
        FieldConverter fieldConverter,
        ReferentialIntegrityValidator integrityValidator,
        MigrationPipelineConfiguration config,
        ILogger<MigrationPipeline> logger)
    {
        _sourceReader = sourceReader;
        _targetWriter = targetWriter;
        _auditLog = auditLog;
        _stateStore = stateStore;
        _fieldConverter = fieldConverter;
        _integrityValidator = integrityValidator;
        _config = config;
        _logger = logger;
    }

    /// <summary>
    /// Executes an incremental sync for a single table.
    /// Reads changed records since last checkpoint, converts, writes, validates, and logs.
    /// </summary>
    public async Task<MigrationBatchResult> SyncTableAsync(
        string sourceTable,
        CancellationToken cancellationToken = default)
    {
        var stopwatch = Stopwatch.StartNew();
        var batchCorrelationId = Guid.NewGuid().ToString();

        if (!_config.TableMappings.TryGetValue(sourceTable, out var mapping))
        {
            throw new ArgumentException($"No table mapping found for source table: {sourceTable}");
        }

        LogSyncStarted(_logger, sourceTable, mapping.TargetTable, batchCorrelationId);

        // Get last sync checkpoint (or start from epoch)
        var lastState = await _stateStore.GetLatestAsync(mapping.TargetTable, cancellationToken);
        var sinceTimestamp = lastState?.LastSyncTimestamp ?? DateTimeOffset.MinValue;

        LogReadingChanges(_logger, sourceTable, sinceTimestamp, _config.BatchSize);

        // Read changed records from source
        var sourceRecords = await _sourceReader.ReadChangedRecordsAsync(
            sourceTable, sinceTimestamp, _config.BatchSize, cancellationToken);

        if (sourceRecords.Count == 0)
        {
            LogNoChanges(_logger, sourceTable);
            stopwatch.Stop();
            return new MigrationBatchResult
            {
                BatchCorrelationId = batchCorrelationId,
                TableName = mapping.TargetTable,
                RecordsRead = 0,
                RecordsWritten = 0,
                RecordsFailed = 0,
                ValidationPassed = true,
                Duration = stopwatch.Elapsed
            };
        }

        // Convert all records
        var convertedRecords = new List<ConvertedRecord>();
        var failedRecords = new List<(SourceRecord Record, string Error)>();

        foreach (var source in sourceRecords)
        {
            try
            {
                var converted = _fieldConverter.Convert(source, mapping);
                convertedRecords.Add(converted);
            }
            catch (Exception ex)
            {
                failedRecords.Add((source, ex.Message));
                LogConversionFailed(_logger, sourceTable, source.PrimaryKey, ex.Message);
            }
        }

        // Write converted records to target
        int recordsWritten = 0;
        if (convertedRecords.Count > 0)
        {
            recordsWritten = await _targetWriter.WriteBatchAsync(convertedRecords, cancellationToken);
        }

        // Validate referential integrity
        var validationErrors = await _integrityValidator.ValidateAsync(
            convertedRecords, mapping, cancellationToken);

        bool validationPassed = validationErrors.Count == 0;

        if (!validationPassed)
        {
            LogValidationFailed(_logger, sourceTable, validationErrors.Count);

            // Rollback the batch
            List<string> writtenKeys = [.. convertedRecords.Select(r => r.PrimaryKey)];
            await _targetWriter.RollbackBatchAsync(mapping.TargetTable, writtenKeys, cancellationToken);
            recordsWritten = 0;
        }

        // Update sync state checkpoint
        if (validationPassed && recordsWritten > 0)
        {
            var latestTimestamp = sourceRecords.Max(r => r.SourceTimestamp);
            var newVersion = (lastState?.Version ?? 0) + 1;

            var newState = new MigrationSyncState
            {
                Id = Guid.NewGuid().ToString(),
                TableName = mapping.TargetTable,
                LastSyncTimestamp = latestTimestamp,
                Version = newVersion,
                RecordCount = recordsWritten,
                CreatedAt = DateTimeOffset.UtcNow
            };

            await _stateStore.SaveAsync(newState, cancellationToken);
        }

        // Write audit log entries
        var auditEntries = CreateAuditEntries(
            batchCorrelationId, convertedRecords, failedRecords, mapping, validationPassed);
        await _auditLog.LogBatchAsync(auditEntries, cancellationToken);

        stopwatch.Stop();
        LogSyncCompleted(_logger, sourceTable, sourceRecords.Count, recordsWritten, failedRecords.Count, stopwatch.Elapsed.TotalSeconds);

        return new MigrationBatchResult
        {
            BatchCorrelationId = batchCorrelationId,
            TableName = mapping.TargetTable,
            RecordsRead = sourceRecords.Count,
            RecordsWritten = recordsWritten,
            RecordsFailed = failedRecords.Count,
            ValidationPassed = validationPassed,
            ValidationErrors = validationErrors,
            Duration = stopwatch.Elapsed
        };
    }

    /// <summary>
    /// Rolls back the last sync batch for a table, restoring the previous checkpoint.
    /// </summary>
    public async Task<MigrationSyncState?> RollbackTableAsync(
        string targetTable,
        CancellationToken cancellationToken = default)
    {
        LogRollbackStarted(_logger, targetTable);
        var restoredState = await _stateStore.RollbackAsync(targetTable, cancellationToken);

        if (restoredState is not null)
        {
            LogRollbackCompleted(_logger, targetTable, restoredState.Version);
        }
        else
        {
            LogRollbackNoState(_logger, targetTable);
        }

        return restoredState;
    }

    private List<MigrationAuditEntry> CreateAuditEntries(
        string batchCorrelationId,
        List<ConvertedRecord> convertedRecords,
        List<(SourceRecord Record, string Error)> failedRecords,
        TableMapping mapping,
        bool validationPassed)
    {
        var entries = new List<MigrationAuditEntry>();
        var now = DateTimeOffset.UtcNow;

        // Successful conversions (only if validation passed)
        if (validationPassed)
        {
            foreach (var record in convertedRecords)
            {
                entries.Add(new MigrationAuditEntry
                {
                    Id = Guid.NewGuid().ToString(),
                    BatchCorrelationId = batchCorrelationId,
                    SourceTable = mapping.SourceTable,
                    TargetTable = mapping.TargetTable,
                    PrimaryKey = record.PrimaryKey,
                    ChangeType = record.ChangeType,
                    Success = true,
                    DataRegion = _config.DataRegion,
                    CreatedAt = now
                });
            }
        }
        else
        {
            // Validation failed — mark all as failed
            foreach (var record in convertedRecords)
            {
                entries.Add(new MigrationAuditEntry
                {
                    Id = Guid.NewGuid().ToString(),
                    BatchCorrelationId = batchCorrelationId,
                    SourceTable = mapping.SourceTable,
                    TargetTable = mapping.TargetTable,
                    PrimaryKey = record.PrimaryKey,
                    ChangeType = record.ChangeType,
                    Success = false,
                    ErrorMessage = "Referential integrity validation failed",
                    DataRegion = _config.DataRegion,
                    CreatedAt = now
                });
            }
        }

        // Conversion failures
        foreach (var (record, error) in failedRecords)
        {
            entries.Add(new MigrationAuditEntry
            {
                Id = Guid.NewGuid().ToString(),
                BatchCorrelationId = batchCorrelationId,
                SourceTable = mapping.SourceTable,
                TargetTable = mapping.TargetTable,
                PrimaryKey = record.PrimaryKey,
                ChangeType = record.ChangeType,
                Success = false,
                ErrorMessage = $"Conversion failed: {error}",
                DataRegion = _config.DataRegion,
                CreatedAt = now
            });
        }

        return entries;
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Migration sync started: {SourceTable} → {TargetTable} (batch: {BatchCorrelationId})")]
    private static partial void LogSyncStarted(ILogger logger, string sourceTable, string targetTable, string batchCorrelationId);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Reading changes from {SourceTable} since {SinceTimestamp} (batch size: {BatchSize})")]
    private static partial void LogReadingChanges(ILogger logger, string sourceTable, DateTimeOffset sinceTimestamp, int batchSize);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "No changes found in {SourceTable} since last sync")]
    private static partial void LogNoChanges(ILogger logger, string sourceTable);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Conversion failed for {SourceTable} record {PrimaryKey}: {ErrorMessage}")]
    private static partial void LogConversionFailed(ILogger logger, string sourceTable, string primaryKey, string errorMessage);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Referential integrity validation failed for {SourceTable}: {ErrorCount} errors")]
    private static partial void LogValidationFailed(ILogger logger, string sourceTable, int errorCount);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Migration sync completed: {SourceTable} — read: {RecordsRead}, written: {RecordsWritten}, failed: {RecordsFailed}, duration: {DurationSeconds:F2}s")]
    private static partial void LogSyncCompleted(ILogger logger, string sourceTable, int recordsRead, int recordsWritten, int recordsFailed, double durationSeconds);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Rollback started for table {TargetTable}")]
    private static partial void LogRollbackStarted(ILogger logger, string targetTable);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Rollback completed for table {TargetTable}, restored to version {Version}")]
    private static partial void LogRollbackCompleted(ILogger logger, string targetTable, int version);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Rollback for table {TargetTable}: no previous state to restore")]
    private static partial void LogRollbackNoState(ILogger logger, string targetTable);
}
