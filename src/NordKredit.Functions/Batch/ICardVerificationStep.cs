namespace NordKredit.Functions.Batch;

/// <summary>
/// Step 1 of the daily batch pipeline: card verification.
/// COBOL source: CBTRN01C.cbl:154-250.
/// Regulations: PSD2 Art.97 (transaction authorization), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public interface ICardVerificationStep
{
    Task<CardVerificationResult> RunAsync(CancellationToken cancellationToken = default);
}
