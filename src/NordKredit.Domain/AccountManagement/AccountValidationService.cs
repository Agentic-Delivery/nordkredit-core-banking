namespace NordKredit.Domain.AccountManagement;

/// <summary>
/// Input validation for account management operations.
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760.
/// Consolidates validation logic from multiple COBOL programs into a single service.
/// Business rule: ACCT-BR-002 (account ID input validation).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3 (operational risk), PSD2 Art. 97.
/// </summary>
public static class AccountValidationService
{
    /// <summary>
    /// Validates an account ID input.
    /// COBOL: EDIT-ACCOUNT paragraph — 11 numeric digits, not all zeros.
    /// Business rule: ACCT-BR-002.
    /// </summary>
    /// <param name="accountId">The account ID to validate.</param>
    /// <param name="required">
    /// True for detail/update operations (blank = error).
    /// False for list operations (blank = show all).
    /// </param>
    public static AccountValidationResult ValidateAccountId(string? accountId, bool required)
    {
        if (string.IsNullOrWhiteSpace(accountId))
        {
            return required
                ? AccountValidationResult.Error("Account number not provided")
                : AccountValidationResult.Success();
        }

        if (!IsValidNumericField(accountId, 11))
        {
            return AccountValidationResult.Error("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER");
        }

        return AccountValidationResult.Success();
    }

    /// <summary>
    /// Validates an account status transition.
    /// Business rule: ACCT-BR-005.
    /// Valid transitions:
    ///   Active → Dormant, Frozen, Closed
    ///   Dormant → Active, Frozen, Closed
    ///   Frozen → Active, Dormant, Closed
    ///   Closed → (terminal — no transitions allowed)
    /// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97.
    /// </summary>
    public static AccountValidationResult ValidateStatusTransition(AccountStatus current, AccountStatus target)
    {
        if (current == target)
        {
            return AccountValidationResult.Error("Account is already in the requested status");
        }

        if (current == AccountStatus.Closed)
        {
            return AccountValidationResult.Error("Cannot transition from Closed status — terminal state");
        }

        return AccountValidationResult.Success();
    }

    /// <summary>
    /// Checks if a value is exactly the expected length and all ASCII digits, and not all zeros.
    /// COBOL: IS NUMERIC check + length validation + zeros check.
    /// </summary>
    private static bool IsValidNumericField(string value, int expectedLength) =>
        value.Length == expectedLength
        && value.All(char.IsAsciiDigit)
        && !value.All(c => c == '0');
}
