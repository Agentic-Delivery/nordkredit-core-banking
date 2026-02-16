namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Result of card input validation (account number or card number).
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-724, COCRDUPC.cbl:721-800.
/// Consolidates duplicated validation logic from three COBOL programs into a single reusable result type.
/// Regulations: FFFS 2014:5 Ch. 4 §3 (operational risk), PSD2 Art. 97.
/// </summary>
public class CardValidationResult
{
    /// <summary>Whether validation passed with no errors.</summary>
    public bool IsValid { get; }

    /// <summary>Error message if validation failed. Maps to COBOL WS-MESSAGE.</summary>
    public string? ErrorMessage { get; }

    private CardValidationResult(bool isValid, string? errorMessage)
    {
        IsValid = isValid;
        ErrorMessage = errorMessage;
    }

    public static CardValidationResult Success()
        => new(true, null);

    public static CardValidationResult Error(string errorMessage)
        => new(false, errorMessage);
}
