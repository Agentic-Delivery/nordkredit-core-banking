using System.Globalization;
using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Generates transaction IDs using a database sequence for thread safety.
/// Replaces COBOL STARTBR/READPREV/ADD 1 pattern (COTRN02C.cbl:442-449).
/// The COBOL approach is not atomic — two concurrent users could read the same
/// last ID and generate duplicates. Database sequences are atomic.
/// Regulation: FFFS 2014:5 Ch.8 — unique, auditable transaction identifiers.
/// </summary>
public class SqlTransactionIdGenerator : ITransactionIdGenerator
{
    private readonly NordKreditDbContext _dbContext;

    public SqlTransactionIdGenerator(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    /// <summary>
    /// Gets the next transaction ID from the database sequence, zero-padded to 16 characters.
    /// COBOL: WS-TRAN-ID-N PIC 9(16).
    /// </summary>
    public async Task<string> GenerateNextIdAsync(CancellationToken cancellationToken = default)
    {
        // Use raw SQL to get next value from the database sequence
        var connection = _dbContext.Database.GetDbConnection();
        await _dbContext.Database.OpenConnectionAsync(cancellationToken);
        try
        {
            using var command = connection.CreateCommand();
            command.CommandText = "SELECT NEXT VALUE FOR TransactionIdSequence";
            var result = await command.ExecuteScalarAsync(cancellationToken);
            var nextId = Convert.ToInt64(result, CultureInfo.InvariantCulture);
            return nextId.ToString("D16", CultureInfo.InvariantCulture);
        }
        finally
        {
            await _dbContext.Database.CloseConnectionAsync();
        }
    }
}
