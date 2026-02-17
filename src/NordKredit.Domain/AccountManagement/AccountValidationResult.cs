namespace NordKredit.Domain.AccountManagement;

/// <summary>
/// Result of account management input validation.
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760.
/// Business rule: ACCT-BR-002 (account ID input validation).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97.
/// </summary>
public class AccountValidationResult
{
    /// <summary>Whether validation passed with no errors.</summary>
    public bool IsValid { get; }

    /// <summary>Error message if validation failed. Maps to COBOL WS-MESSAGE.</summary>
    public string? ErrorMessage { get; }

    private AccountValidationResult(bool isValid, string? errorMessage)
    {
        IsValid = isValid;
        ErrorMessage = errorMessage;
    }

    public static AccountValidationResult Success()
        => new(true, null);

    public static AccountValidationResult Error(string errorMessage)
        => new(false, errorMessage);
}
