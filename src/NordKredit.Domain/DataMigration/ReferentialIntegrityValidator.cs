namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Validates referential integrity after a sync batch by checking that
/// all foreign key references in the migrated records point to existing records.
/// COBOL source: VSAM alternate index relationships (e.g., CARDDAT→ACCTFILE).
/// Regulation: FFFS 2014:5 Ch.4 §3 — operational risk (data integrity).
/// </summary>
public class ReferentialIntegrityValidator
{
    private readonly IReferentialIntegrityChecker _checker;

    public ReferentialIntegrityValidator(IReferentialIntegrityChecker checker)
    {
        _checker = checker;
    }

    /// <summary>
    /// Validates referential integrity for a batch of converted records against the table mapping.
    /// Returns a list of validation error messages (empty if all valid).
    /// </summary>
    public async Task<IReadOnlyList<string>> ValidateAsync(
        IReadOnlyList<ConvertedRecord> records,
        TableMapping mapping,
        CancellationToken cancellationToken = default)
    {
        var errors = new List<string>();

        if (mapping.ForeignKeys.Count == 0)
        {
            return errors;
        }

        foreach (var fk in mapping.ForeignKeys)
        {
            // Collect all distinct FK values from the batch
            var fkValues = new HashSet<string>();
            foreach (var record in records)
            {
                if (record.ChangeType == ChangeType.Delete)
                {
                    continue;
                }

                if (record.Fields.TryGetValue(fk.Column, out var value) && value is not null)
                {
                    fkValues.Add(value.ToString()!);
                }
            }

            if (fkValues.Count == 0)
            {
                continue;
            }

            var missingKeys = await _checker.FindMissingKeysAsync(
                fk.ReferencedTable, fk.ReferencedColumn, fkValues, cancellationToken);

            foreach (var missingKey in missingKeys)
            {
                errors.Add(
                    $"Referential integrity violation: {mapping.TargetTable}.{fk.Column} " +
                    $"value '{missingKey}' not found in {fk.ReferencedTable}.{fk.ReferencedColumn}");
            }
        }

        return errors;
    }
}
