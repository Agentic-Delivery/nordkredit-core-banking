using NordKredit.Domain.DataMigration;

namespace NordKredit.UnitTests.DataMigration;

/// <summary>
/// Unit tests for ReferentialIntegrityValidator — validates foreign key relationships
/// after a sync batch.
/// COBOL source: VSAM alternate index relationships (CARDDAT→ACCTFILE, TRANSACT→CARDDAT).
/// Regulation: FFFS 2014:5 Ch.4 §3 — operational risk (data integrity).
/// </summary>
public class ReferentialIntegrityValidatorTests
{
    private readonly StubReferentialIntegrityChecker _checker = new();
    private readonly ReferentialIntegrityValidator _validator;

    public ReferentialIntegrityValidatorTests()
    {
        _validator = new ReferentialIntegrityValidator(_checker);
    }

    // ===================================================================
    // AC: Referential integrity validation runs post-sync
    // No FK constraints → no errors
    // ===================================================================

    [Fact]
    public async Task Validate_NoForeignKeys_ReturnsEmpty()
    {
        var mapping = CreateMapping(foreignKeys: []);
        var records = new List<ConvertedRecord> { CreateConvertedRecord() };

        var errors = await _validator.ValidateAsync(records, mapping);

        Assert.Empty(errors);
    }

    // ===================================================================
    // AC: All FK references exist → validation passes
    // ===================================================================

    [Fact]
    public async Task Validate_AllReferencesExist_ReturnsEmpty()
    {
        var mapping = CreateMapping(foreignKeys:
        [
            new ForeignKeyMapping { Column = "AccountId", ReferencedTable = "Accounts", ReferencedColumn = "Id" }
        ]);
        var records = new List<ConvertedRecord>
        {
            CreateConvertedRecord(fields: new() { ["AccountId"] = "00012345678" })
        };
        _checker.MissingKeys = [];

        var errors = await _validator.ValidateAsync(records, mapping);

        Assert.Empty(errors);
    }

    // ===================================================================
    // AC: Missing FK reference → returns validation error
    // ===================================================================

    [Fact]
    public async Task Validate_MissingReference_ReturnsError()
    {
        var mapping = CreateMapping(
            targetTable: "Cards",
            foreignKeys:
            [
                new ForeignKeyMapping { Column = "AccountId", ReferencedTable = "Accounts", ReferencedColumn = "Id" }
            ]);
        var records = new List<ConvertedRecord>
        {
            CreateConvertedRecord(fields: new() { ["AccountId"] = "MISSING001" })
        };
        _checker.MissingKeys = ["MISSING001"];

        var errors = await _validator.ValidateAsync(records, mapping);

        Assert.Single(errors);
        Assert.Contains("MISSING001", errors[0]);
        Assert.Contains("Accounts", errors[0]);
    }

    // ===================================================================
    // AC: Delete records are skipped during FK validation
    // ===================================================================

    [Fact]
    public async Task Validate_DeleteRecord_SkipsValidation()
    {
        var mapping = CreateMapping(foreignKeys:
        [
            new ForeignKeyMapping { Column = "AccountId", ReferencedTable = "Accounts", ReferencedColumn = "Id" }
        ]);
        var records = new List<ConvertedRecord>
        {
            CreateConvertedRecord(changeType: ChangeType.Delete, fields: new() { ["AccountId"] = "00012345678" })
        };

        var errors = await _validator.ValidateAsync(records, mapping);

        Assert.Empty(errors);
        Assert.False(_checker.WasCalled);
    }

    // ===================================================================
    // AC: Null FK value is skipped
    // ===================================================================

    [Fact]
    public async Task Validate_NullFkValue_SkipsValidation()
    {
        var mapping = CreateMapping(foreignKeys:
        [
            new ForeignKeyMapping { Column = "AccountId", ReferencedTable = "Accounts", ReferencedColumn = "Id" }
        ]);
        var records = new List<ConvertedRecord>
        {
            CreateConvertedRecord(fields: new() { ["AccountId"] = null })
        };

        var errors = await _validator.ValidateAsync(records, mapping);

        Assert.Empty(errors);
    }

    // ===================================================================
    // AC: Multiple FK violations reported
    // ===================================================================

    [Fact]
    public async Task Validate_MultipleMissingReferences_ReturnsAllErrors()
    {
        var mapping = CreateMapping(
            targetTable: "Transactions",
            foreignKeys:
            [
                new ForeignKeyMapping { Column = "CardNumber", ReferencedTable = "Cards", ReferencedColumn = "CardNumber" }
            ]);
        var records = new List<ConvertedRecord>
        {
            CreateConvertedRecord(fields: new() { ["CardNumber"] = "CARD001" }),
            CreateConvertedRecord(fields: new() { ["CardNumber"] = "CARD002" })
        };
        _checker.MissingKeys = ["CARD001", "CARD002"];

        var errors = await _validator.ValidateAsync(records, mapping);

        Assert.Equal(2, errors.Count);
    }

    // ===================================================================
    // AC: Deduplicates FK values before checking
    // ===================================================================

    [Fact]
    public async Task Validate_DuplicateFkValues_DeduplicatesBeforeCheck()
    {
        var mapping = CreateMapping(foreignKeys:
        [
            new ForeignKeyMapping { Column = "AccountId", ReferencedTable = "Accounts", ReferencedColumn = "Id" }
        ]);
        var records = new List<ConvertedRecord>
        {
            CreateConvertedRecord(fields: new() { ["AccountId"] = "ACCT001" }),
            CreateConvertedRecord(fields: new() { ["AccountId"] = "ACCT001" })
        };
        _checker.MissingKeys = [];

        await _validator.ValidateAsync(records, mapping);

        // Checker should have received deduplicated set
        Assert.Single(_checker.LastCheckedValues!);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static TableMapping CreateMapping(
        string targetTable = "TestTable",
        IReadOnlyList<ForeignKeyMapping>? foreignKeys = null) =>
        new()
        {
            SourceTable = "SOURCE",
            TargetTable = targetTable,
            PrimaryKeyColumns = ["Id"],
            Fields = [],
            ForeignKeys = foreignKeys ?? []
        };

    private static ConvertedRecord CreateConvertedRecord(
        ChangeType changeType = ChangeType.Insert,
        Dictionary<string, object?>? fields = null) =>
        new()
        {
            TargetTable = "TestTable",
            ChangeType = changeType,
            PrimaryKey = "PK001",
            Fields = fields ?? [],
            SourceTimestamp = DateTimeOffset.UtcNow
        };
}

// ===================================================================
// Test doubles
// ===================================================================

internal sealed class StubReferentialIntegrityChecker : IReferentialIntegrityChecker
{
    public IReadOnlyList<string> MissingKeys { get; set; } = [];
    public bool WasCalled { get; private set; }
    public IReadOnlyCollection<string>? LastCheckedValues { get; private set; }

    public Task<IReadOnlyList<string>> FindMissingKeysAsync(
        string tableName, string columnName,
        IReadOnlyCollection<string> keyValues,
        CancellationToken cancellationToken = default)
    {
        WasCalled = true;
        LastCheckedValues = keyValues;
        return Task.FromResult(MissingKeys);
    }
}
