namespace NordKredit.Domain.Lending;

/// <summary>
/// Result of lending domain input validation.
/// COBOL source: CBTRN02C.cbl:370-421 (WS-VALIDATION-FAIL-REASON, WS-VALIDATION-FAIL-REASON-DESC).
/// Business rules: LND-BR-002, LND-BR-009.
/// Regulations: FSA FFFS 2014:5 Ch. 6, PSD2 Art. 64.
/// </summary>
public class LoanValidationResult
{
    /// <summary>Whether validation passed with no errors.</summary>
    public bool IsValid { get; }

    /// <summary>Error message if validation failed. Maps to COBOL WS-VALIDATION-FAIL-REASON-DESC.</summary>
    public string? ErrorMessage { get; }

    private LoanValidationResult(bool isValid, string? errorMessage)
    {
        IsValid = isValid;
        ErrorMessage = errorMessage;
    }

    public static LoanValidationResult Success()
        => new(true, null);

    public static LoanValidationResult Error(string errorMessage)
        => new(false, errorMessage);
}
