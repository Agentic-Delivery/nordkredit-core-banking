namespace NordKredit.Domain.Deposits;

/// <summary>
/// Result of deposit account input validation.
/// COBOL source: CVACT01Y.cpy — account field validation.
/// Business rule: DEP-BR-001 (deposit account data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97.
/// </summary>
public class DepositValidationResult
{
    /// <summary>Whether validation passed with no errors.</summary>
    public bool IsValid { get; }

    /// <summary>Error message if validation failed.</summary>
    public string? ErrorMessage { get; }

    private DepositValidationResult(bool isValid, string? errorMessage)
    {
        IsValid = isValid;
        ErrorMessage = errorMessage;
    }

    public static DepositValidationResult Success()
        => new(true, null);

    public static DepositValidationResult Error(string errorMessage)
        => new(false, errorMessage);
}
