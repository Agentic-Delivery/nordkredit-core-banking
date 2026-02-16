namespace NordKredit.Domain.Transactions;

/// <summary>
/// Generates unique transaction IDs.
/// COBOL source: COTRN02C.cbl:442-449 — replaces STARTBR/READPREV/ADD 1 pattern.
/// Uses database sequence for thread safety (COBOL READPREV + WRITE is not atomic).
/// Regulation: FFFS 2014:5 Ch.8 — unique, auditable transaction identifiers.
/// </summary>
public interface ITransactionIdGenerator
{
    /// <summary>
    /// Returns the next transaction ID as a 16-character zero-padded string.
    /// COBOL: WS-TRAN-ID-N PIC 9(16).
    /// </summary>
    Task<string> GenerateNextIdAsync(CancellationToken cancellationToken = default);
}
