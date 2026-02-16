namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Result of card update field validation that collects ALL errors simultaneously.
/// Improvement over COBOL: COCRDUPC.cbl only reports the first error via WS-RETURN-MSG-OFF flag.
/// The migrated system validates all fields and returns every error at once.
/// COBOL source: COCRDUPC.cbl:806-947.
/// Regulations: FFFS 2014:5 Ch. 4 §3 (operational risk), PSD2 Art. 97, GDPR Art. 5(1)(d).
/// </summary>
public class CardUpdateValidationResult
{
    /// <summary>Whether all field validations passed with no errors.</summary>
    public bool IsValid => Errors.Count == 0;

    /// <summary>
    /// All validation error messages. Empty when valid.
    /// COBOL reported only the first error; this returns all errors simultaneously.
    /// </summary>
    public IReadOnlyList<string> Errors { get; }

    private CardUpdateValidationResult(IReadOnlyList<string> errors)
    {
        Errors = errors;
    }

    public static CardUpdateValidationResult Success()
        => new([]);

    public static CardUpdateValidationResult WithErrors(IReadOnlyList<string> errors)
        => new(errors);
}
