namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for daily transaction rejection records.
/// COBOL source: CBTRN02C.cbl:370-422 — writes rejected records with fail codes.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk).
/// </summary>
public interface IDailyRejectRepository
{
    Task AddAsync(DailyReject reject, CancellationToken cancellationToken = default);
}
