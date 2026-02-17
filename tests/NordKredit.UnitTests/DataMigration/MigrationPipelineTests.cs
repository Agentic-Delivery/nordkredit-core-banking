using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.DataMigration;

namespace NordKredit.UnitTests.DataMigration;

/// <summary>
/// Unit tests for MigrationPipeline — orchestrates incremental Db2 → Azure SQL sync.
/// Validates incremental sync, conversion, validation, rollback, and audit logging.
/// Regulations: GDPR Art. 5(1)(d) (accuracy), GDPR Art.17 (erasure), DORA Art.11 (audit).
/// </summary>
public class MigrationPipelineTests
{
    private static readonly byte[] _testBytes = [0x01];
    private static readonly byte[] _testBytes2 = [0x02];
    private static readonly byte[] _badBytes = [0xFF];

    private readonly StubSourceDataReader _sourceReader = new();
    private readonly StubTargetDataWriter _targetWriter = new();
    private readonly StubMigrationAuditLog _auditLog = new();
    private readonly StubMigrationStateStore _stateStore = new();
    private readonly StubEbcdicConverterForPipeline _ebcdicConverter = new();
    private readonly StubReferentialIntegrityCheckerForPipeline _integrityChecker = new();

    // ===================================================================
    // AC: Pipeline incrementally syncs data (changed records only)
    // No changes since last sync → no records written
    // ===================================================================

    [Fact]
    public async Task SyncTable_NoChanges_ReturnsZeroRecords()
    {
        _sourceReader.Records = [];
        var pipeline = CreatePipeline();

        var result = await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(0, result.RecordsRead);
        Assert.Equal(0, result.RecordsWritten);
        Assert.True(result.IsSuccess);
    }

    // ===================================================================
    // AC: Pipeline reads changed records and writes to target
    // ===================================================================

    [Fact]
    public async Task SyncTable_WithChanges_ReadsConvertsAndWrites()
    {
        var sourceRecords = new List<SourceRecord>
        {
            CreateSourceRecord("ACCTFILE", "PK001", new() { ["ACCT-ID"] = _testBytes }),
            CreateSourceRecord("ACCTFILE", "PK002", new() { ["ACCT-ID"] = _testBytes2 })
        };
        _sourceReader.Records = sourceRecords;
        _targetWriter.WrittenCount = 2;
        _ebcdicConverter.UnicodeResult = "00012345678";
        var pipeline = CreatePipeline();

        var result = await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(2, result.RecordsRead);
        Assert.Equal(2, result.RecordsWritten);
        Assert.True(result.IsSuccess);
    }

    // ===================================================================
    // AC: Pipeline supports incremental sync (uses last checkpoint)
    // ===================================================================

    [Fact]
    public async Task SyncTable_WithExistingCheckpoint_ReadsSinceLastSync()
    {
        var lastSync = new DateTimeOffset(2026, 2, 16, 12, 0, 0, TimeSpan.Zero);
        _stateStore.LatestState = new MigrationSyncState
        {
            Id = "state-1",
            TableName = "Accounts",
            LastSyncTimestamp = lastSync,
            Version = 1,
            RecordCount = 100,
            CreatedAt = lastSync
        };
        _sourceReader.Records = [];
        var pipeline = CreatePipeline();

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(lastSync, _sourceReader.LastSinceTimestamp);
    }

    // ===================================================================
    // AC: First sync starts from beginning of time
    // ===================================================================

    [Fact]
    public async Task SyncTable_NoCheckpoint_StartFromMinValue()
    {
        _stateStore.LatestState = null;
        _sourceReader.Records = [];
        var pipeline = CreatePipeline();

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(DateTimeOffset.MinValue, _sourceReader.LastSinceTimestamp);
    }

    // ===================================================================
    // AC: Sync state checkpoint is saved after successful sync
    // ===================================================================

    [Fact]
    public async Task SyncTable_Success_SavesCheckpoint()
    {
        var sourceTimestamp = new DateTimeOffset(2026, 2, 17, 10, 0, 0, TimeSpan.Zero);
        _sourceReader.Records =
        [
            CreateSourceRecord("ACCTFILE", "PK001", timestamp: sourceTimestamp)
        ];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        var pipeline = CreatePipeline();

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.NotNull(_stateStore.LastSavedState);
        Assert.Equal("Accounts", _stateStore.LastSavedState.TableName);
        Assert.Equal(sourceTimestamp, _stateStore.LastSavedState.LastSyncTimestamp);
        Assert.Equal(1, _stateStore.LastSavedState.Version);
    }

    // ===================================================================
    // AC: Checkpoint version increments on each sync
    // ===================================================================

    [Fact]
    public async Task SyncTable_ExistingCheckpoint_IncrementsVersion()
    {
        _stateStore.LatestState = new MigrationSyncState
        {
            Id = "state-1",
            TableName = "Accounts",
            LastSyncTimestamp = DateTimeOffset.UtcNow.AddHours(-1),
            Version = 3,
            RecordCount = 50,
            CreatedAt = DateTimeOffset.UtcNow.AddHours(-1)
        };
        _sourceReader.Records = [CreateSourceRecord("ACCTFILE", "PK001")];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        var pipeline = CreatePipeline();

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(4, _stateStore.LastSavedState!.Version);
    }

    // ===================================================================
    // AC: Conversion failures are counted but don't stop the batch
    // ===================================================================

    [Fact]
    public async Task SyncTable_ConversionFailure_ContinuesWithOtherRecords()
    {
        _sourceReader.Records =
        [
            CreateSourceRecord("ACCTFILE", "PK001", new() { ["BAD-FIELD"] = _badBytes }),
            CreateSourceRecord("ACCTFILE", "PK002", new() { ["ACCT-ID"] = _testBytes })
        ];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        _ebcdicConverter.ThrowOnFieldName = "BAD-FIELD";
        var pipeline = CreatePipeline(fields:
        [
            new FieldMapping { SourceField = "BAD-FIELD", TargetColumn = "BadCol", FieldType = CobolFieldType.PackedDecimal },
            new FieldMapping { SourceField = "ACCT-ID", TargetColumn = "Id", FieldType = CobolFieldType.NumericString }
        ]);

        var result = await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(2, result.RecordsRead);
        Assert.Equal(1, result.RecordsWritten);
        Assert.Equal(1, result.RecordsFailed);
    }

    // ===================================================================
    // AC: Referential integrity validation runs post-sync
    // Validation failure triggers rollback
    // ===================================================================

    [Fact]
    public async Task SyncTable_ValidationFails_RollsBackBatch()
    {
        _sourceReader.Records = [CreateSourceRecord("ACCTFILE", "PK001")];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        _integrityChecker.MissingKeys = ["MISSING001"];
        var pipeline = CreatePipeline(foreignKeys:
        [
            new ForeignKeyMapping { Column = "Id", ReferencedTable = "OtherTable", ReferencedColumn = "Id" }
        ]);

        var result = await pipeline.SyncTableAsync("ACCTFILE");

        Assert.False(result.ValidationPassed);
        Assert.Equal(0, result.RecordsWritten); // Rolled back
        Assert.True(_targetWriter.RollbackCalled);
    }

    // ===================================================================
    // AC: Validation failure does not save checkpoint
    // ===================================================================

    [Fact]
    public async Task SyncTable_ValidationFails_DoesNotSaveCheckpoint()
    {
        _sourceReader.Records = [CreateSourceRecord("ACCTFILE", "PK001")];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        _integrityChecker.MissingKeys = ["MISSING001"];
        var pipeline = CreatePipeline(foreignKeys:
        [
            new ForeignKeyMapping { Column = "Id", ReferencedTable = "OtherTable", ReferencedColumn = "Id" }
        ]);

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Null(_stateStore.LastSavedState);
    }

    // ===================================================================
    // AC: Audit logging tracks all data changes (GDPR/DORA)
    // ===================================================================

    [Fact]
    public async Task SyncTable_Success_LogsAuditEntries()
    {
        _sourceReader.Records =
        [
            CreateSourceRecord("ACCTFILE", "PK001"),
            CreateSourceRecord("ACCTFILE", "PK002")
        ];
        _targetWriter.WrittenCount = 2;
        _ebcdicConverter.UnicodeResult = "test";
        var pipeline = CreatePipeline();

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Equal(2, _auditLog.LoggedEntries.Count);
        Assert.All(_auditLog.LoggedEntries, e =>
        {
            Assert.True(e.Success);
            Assert.Equal("Sweden Central", e.DataRegion);
            Assert.Equal("ACCTFILE", e.SourceTable);
            Assert.Equal("Accounts", e.TargetTable);
        });
    }

    // ===================================================================
    // AC: Data residency is maintained (EU/Sweden Central)
    // ===================================================================

    [Fact]
    public async Task SyncTable_AuditEntries_HaveCorrectDataRegion()
    {
        _sourceReader.Records = [CreateSourceRecord("ACCTFILE", "PK001")];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        var pipeline = CreatePipeline();

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.All(_auditLog.LoggedEntries, e => Assert.Equal("Sweden Central", e.DataRegion));
    }

    // ===================================================================
    // AC: Failed records are logged with error details
    // ===================================================================

    [Fact]
    public async Task SyncTable_FailedConversion_LogsErrorInAudit()
    {
        _sourceReader.Records =
        [
            CreateSourceRecord("ACCTFILE", "PK001", new() { ["BAD-FIELD"] = _badBytes })
        ];
        _ebcdicConverter.ThrowOnFieldName = "BAD-FIELD";
        var pipeline = CreatePipeline(fields:
        [
            new FieldMapping { SourceField = "BAD-FIELD", TargetColumn = "BadCol", FieldType = CobolFieldType.PackedDecimal }
        ]);

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Single(_auditLog.LoggedEntries);
        Assert.False(_auditLog.LoggedEntries[0].Success);
        Assert.Contains("Conversion failed", _auditLog.LoggedEntries[0].ErrorMessage!);
    }

    // ===================================================================
    // AC: Rollback capability exists (revert to previous sync state)
    // ===================================================================

    [Fact]
    public async Task RollbackTable_RestoresPreviousState()
    {
        var previousState = new MigrationSyncState
        {
            Id = "state-1",
            TableName = "Accounts",
            LastSyncTimestamp = DateTimeOffset.UtcNow.AddHours(-2),
            Version = 1,
            RecordCount = 50,
            CreatedAt = DateTimeOffset.UtcNow.AddHours(-2)
        };
        _stateStore.RollbackResult = previousState;
        var pipeline = CreatePipeline();

        var result = await pipeline.RollbackTableAsync("Accounts");

        Assert.NotNull(result);
        Assert.Equal(1, result.Version);
        Assert.True(_stateStore.RollbackCalled);
    }

    // ===================================================================
    // AC: Rollback with no previous state returns null
    // ===================================================================

    [Fact]
    public async Task RollbackTable_NoPreviousState_ReturnsNull()
    {
        _stateStore.RollbackResult = null;
        var pipeline = CreatePipeline();

        var result = await pipeline.RollbackTableAsync("Accounts");

        Assert.Null(result);
    }

    // ===================================================================
    // Unknown table mapping throws
    // ===================================================================

    [Fact]
    public async Task SyncTable_UnknownTable_ThrowsArgumentException()
    {
        var pipeline = CreatePipeline();

        await Assert.ThrowsAsync<ArgumentException>(
            () => pipeline.SyncTableAsync("UNKNOWN_TABLE"));
    }

    // ===================================================================
    // AC: Batch correlation ID links all audit entries
    // ===================================================================

    [Fact]
    public async Task SyncTable_AllAuditEntries_HaveSameBatchCorrelationId()
    {
        _sourceReader.Records =
        [
            CreateSourceRecord("ACCTFILE", "PK001"),
            CreateSourceRecord("ACCTFILE", "PK002")
        ];
        _targetWriter.WrittenCount = 2;
        _ebcdicConverter.UnicodeResult = "test";
        var pipeline = CreatePipeline();

        var result = await pipeline.SyncTableAsync("ACCTFILE");

        var batchId = result.BatchCorrelationId;
        Assert.All(_auditLog.LoggedEntries, e => Assert.Equal(batchId, e.BatchCorrelationId));
    }

    // ===================================================================
    // AC: Validation failure audit entries record failure reason
    // ===================================================================

    [Fact]
    public async Task SyncTable_ValidationFails_AuditEntriesMarkAsFailedWithReason()
    {
        _sourceReader.Records = [CreateSourceRecord("ACCTFILE", "PK001")];
        _targetWriter.WrittenCount = 1;
        _ebcdicConverter.UnicodeResult = "test";
        _integrityChecker.MissingKeys = ["MISSING001"];
        var pipeline = CreatePipeline(foreignKeys:
        [
            new ForeignKeyMapping { Column = "Id", ReferencedTable = "OtherTable", ReferencedColumn = "Id" }
        ]);

        await pipeline.SyncTableAsync("ACCTFILE");

        Assert.Single(_auditLog.LoggedEntries);
        Assert.False(_auditLog.LoggedEntries[0].Success);
        Assert.Contains("Referential integrity", _auditLog.LoggedEntries[0].ErrorMessage!);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private MigrationPipeline CreatePipeline(
        IReadOnlyList<FieldMapping>? fields = null,
        IReadOnlyList<ForeignKeyMapping>? foreignKeys = null)
    {
        var mapping = new TableMapping
        {
            SourceTable = "ACCTFILE",
            TargetTable = "Accounts",
            PrimaryKeyColumns = ["Id"],
            Fields = fields ??
            [
                new FieldMapping { SourceField = "ACCT-ID", TargetColumn = "Id", FieldType = CobolFieldType.NumericString }
            ],
            ForeignKeys = foreignKeys ?? []
        };

        var config = new MigrationPipelineConfiguration
        {
            BatchSize = 1000,
            DataRegion = "Sweden Central",
            TableMappings = new Dictionary<string, TableMapping> { ["ACCTFILE"] = mapping }
        };

        var fieldConverter = new FieldConverter(_ebcdicConverter);
        var integrityValidator = new ReferentialIntegrityValidator(_integrityChecker);

        return new MigrationPipeline(
            _sourceReader,
            _targetWriter,
            _auditLog,
            _stateStore,
            fieldConverter,
            integrityValidator,
            config,
            NullLogger<MigrationPipeline>.Instance);
    }

    private static SourceRecord CreateSourceRecord(
        string tableName,
        string primaryKey,
        Dictionary<string, object>? fields = null,
        DateTimeOffset? timestamp = null) =>
        new()
        {
            TableName = tableName,
            ChangeType = ChangeType.Insert,
            PrimaryKey = primaryKey,
            Fields = fields ?? new Dictionary<string, object> { ["ACCT-ID"] = _testBytes },
            SourceTimestamp = timestamp ?? DateTimeOffset.UtcNow
        };
}

// ===================================================================
// Test doubles for MigrationPipeline
// ===================================================================

internal sealed class StubSourceDataReader : ISourceDataReader
{
    public IReadOnlyList<SourceRecord> Records { get; set; } = [];
    public DateTimeOffset LastSinceTimestamp { get; private set; }

    public Task<IReadOnlyList<SourceRecord>> ReadChangedRecordsAsync(
        string tableName, DateTimeOffset sinceTimestamp, int batchSize,
        CancellationToken cancellationToken = default)
    {
        LastSinceTimestamp = sinceTimestamp;
        return Task.FromResult(Records);
    }
}

internal sealed class StubTargetDataWriter : ITargetDataWriter
{
    public int WrittenCount { get; set; }
    public bool RollbackCalled { get; private set; }

    public Task<int> WriteBatchAsync(
        IReadOnlyList<ConvertedRecord> records,
        CancellationToken cancellationToken = default) =>
        Task.FromResult(WrittenCount);

    public Task RollbackBatchAsync(
        string tableName, IReadOnlyList<string> primaryKeys,
        CancellationToken cancellationToken = default)
    {
        RollbackCalled = true;
        return Task.CompletedTask;
    }
}

internal sealed class StubMigrationAuditLog : IMigrationAuditLog
{
    public List<MigrationAuditEntry> LoggedEntries { get; } = [];

    public Task LogBatchAsync(
        IReadOnlyList<MigrationAuditEntry> entries,
        CancellationToken cancellationToken = default)
    {
        LoggedEntries.AddRange(entries);
        return Task.CompletedTask;
    }
}

internal sealed class StubMigrationStateStore : IMigrationStateStore
{
    public MigrationSyncState? LatestState { get; set; }
    public MigrationSyncState? LastSavedState { get; private set; }
    public MigrationSyncState? RollbackResult { get; set; }
    public bool RollbackCalled { get; private set; }

    public Task<MigrationSyncState?> GetLatestAsync(
        string tableName, CancellationToken cancellationToken = default) =>
        Task.FromResult(LatestState);

    public Task SaveAsync(
        MigrationSyncState state, CancellationToken cancellationToken = default)
    {
        LastSavedState = state;
        return Task.CompletedTask;
    }

    public Task<MigrationSyncState?> RollbackAsync(
        string tableName, CancellationToken cancellationToken = default)
    {
        RollbackCalled = true;
        return Task.FromResult(RollbackResult);
    }
}

internal sealed class StubEbcdicConverterForPipeline : IEbcdicConverter
{
    public string UnicodeResult { get; set; } = "";
    public decimal PackedDecimalResult { get; set; }
    public decimal ZonedDecimalResult { get; set; }
    public string? ThrowOnFieldName { get; set; }

    public string ConvertToUnicode(byte[] ebcdicBytes) => UnicodeResult;

    public decimal ConvertPackedDecimal(byte[] packedBytes, int scale = 0)
    {
        if (ThrowOnFieldName is not null)
        {
            throw new InvalidOperationException("Invalid packed decimal data");
        }

        return PackedDecimalResult;
    }

    public decimal ConvertZonedDecimal(byte[] zonedBytes, int scale = 0) => ZonedDecimalResult;
}

internal sealed class StubReferentialIntegrityCheckerForPipeline : IReferentialIntegrityChecker
{
    public IReadOnlyList<string> MissingKeys { get; set; } = [];

    public Task<IReadOnlyList<string>> FindMissingKeysAsync(
        string tableName, string columnName,
        IReadOnlyCollection<string> keyValues,
        CancellationToken cancellationToken = default) =>
        Task.FromResult(MissingKeys);
}
