namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of transaction add validation.
/// COBOL source: COTRN02C.cbl — maps to WS-ERR-FLG and WS-MESSAGE.
/// Sequential validation exits on first error, matching COBOL PERFORM SEND-TRNADD-SCREEN behavior.
/// </summary>
public class TransactionValidationResult
{
    /// <summary>Whether validation passed with no errors.</summary>
    public bool IsValid { get; }

    /// <summary>Whether the user needs to confirm the transaction (Confirm = 'Y').</summary>
    public bool ConfirmationRequired { get; }

    /// <summary>Error message if validation failed. Maps to COBOL WS-MESSAGE.</summary>
    public string? ErrorMessage { get; }

    /// <summary>Card number resolved from cross-reference (when Account ID was provided).</summary>
    public string? ResolvedCardNumber { get; }

    /// <summary>Account ID resolved from cross-reference (when Card Number was provided).</summary>
    public string? ResolvedAccountId { get; }

    private TransactionValidationResult(bool isValid, bool confirmationRequired, string? errorMessage, string? resolvedCardNumber, string? resolvedAccountId)
    {
        IsValid = isValid;
        ConfirmationRequired = confirmationRequired;
        ErrorMessage = errorMessage;
        ResolvedCardNumber = resolvedCardNumber;
        ResolvedAccountId = resolvedAccountId;
    }

    public static TransactionValidationResult Success(string resolvedCardNumber, string resolvedAccountId)
        => new(true, false, null, resolvedCardNumber, resolvedAccountId);

    public static TransactionValidationResult Error(string errorMessage)
        => new(false, false, errorMessage, null, null);

    public static TransactionValidationResult NeedsConfirmation(string resolvedCardNumber, string resolvedAccountId)
        => new(false, true, null, resolvedCardNumber, resolvedAccountId);
}
