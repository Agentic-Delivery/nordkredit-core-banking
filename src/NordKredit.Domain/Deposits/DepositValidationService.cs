namespace NordKredit.Domain.Deposits;

/// <summary>
/// Input validation for deposit account operations.
/// COBOL source: CVACT01Y.cpy — account field validation.
/// Business rule: DEP-BR-001 (deposit account data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97.
/// </summary>
public static class DepositValidationService
{
    /// <summary>
    /// Validates an account ID input (11 numeric digits, not all zeros).
    /// COBOL: ACCT-ID PIC 9(11).
    /// </summary>
    public static DepositValidationResult ValidateAccountId(string? accountId)
    {
        if (string.IsNullOrWhiteSpace(accountId))
        {
            return DepositValidationResult.Error("Account number not provided");
        }

        if (!IsValidNumericField(accountId, 11))
        {
            return DepositValidationResult.Error("Account number must be an 11-digit number");
        }

        return DepositValidationResult.Success();
    }

    /// <summary>
    /// Validates a deposit account status transition.
    /// Business rule: DEP-BR-009 (dormancy management).
    /// Valid transitions:
    ///   Active → Dormant, Frozen, Closed
    ///   Dormant → Active, Frozen, Closed
    ///   Frozen → Active, Dormant, Closed
    ///   Closed → (terminal — no transitions)
    /// </summary>
    public static DepositValidationResult ValidateStatusTransition(
        DepositAccountStatus current,
        DepositAccountStatus target)
    {
        if (current == target)
        {
            return DepositValidationResult.Error("Account is already in the requested status");
        }

        if (current == DepositAccountStatus.Closed)
        {
            return DepositValidationResult.Error("Cannot transition from Closed status — terminal state");
        }

        return DepositValidationResult.Success();
    }

    /// <summary>
    /// Validates a disclosure group ID.
    /// COBOL: ACCT-GROUP-ID PIC X(10).
    /// </summary>
    public static DepositValidationResult ValidateDisclosureGroupId(string? groupId)
    {
        if (string.IsNullOrWhiteSpace(groupId))
        {
            return DepositValidationResult.Error("Disclosure group ID not provided");
        }

        if (groupId.Length > 10)
        {
            return DepositValidationResult.Error("Disclosure group ID must not exceed 10 characters");
        }

        return DepositValidationResult.Success();
    }

    private static bool IsValidNumericField(string value, int expectedLength) =>
        value.Length == expectedLength
        && value.All(char.IsAsciiDigit)
        && !value.All(c => c == '0');
}
