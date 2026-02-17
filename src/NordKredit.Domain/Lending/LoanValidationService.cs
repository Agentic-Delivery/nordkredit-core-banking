namespace NordKredit.Domain.Lending;

/// <summary>
/// Input validation for lending domain operations.
/// COBOL source: CBTRN02C.cbl:370-421 (1500-VALIDATE-TRAN), COCRDLIC.cbl:1003-1034.
/// Business rules: LND-BR-002 (credit limit enforcement), LND-BR-009 (expiration enforcement).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), PSD2 Art. 64 (transaction integrity).
/// </summary>
public static class LoanValidationService
{
    /// <summary>
    /// Maximum value for COBOL PIC S9(10)V99 fields.
    /// </summary>
    private const decimal _maxCobolAmount = 9999999999.99m;

    /// <summary>
    /// Validates a loan account ID input.
    /// COBOL: EDIT-ACCOUNT paragraph — 11 numeric digits, not all zeros.
    /// </summary>
    public static LoanValidationResult ValidateAccountId(string? accountId)
    {
        if (string.IsNullOrWhiteSpace(accountId))
        {
            return LoanValidationResult.Error("Account number not provided");
        }

        if (!IsValidNumericField(accountId, 11))
        {
            return LoanValidationResult.Error("ACCOUNT ID MUST BE A 11 DIGIT NUMBER");
        }

        return LoanValidationResult.Success();
    }

    /// <summary>
    /// Validates a credit limit amount.
    /// COBOL: PIC S9(10)V99 — must be positive and within COBOL field capacity.
    /// Business rule: LND-BR-002.
    /// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk management).
    /// </summary>
    public static LoanValidationResult ValidateCreditLimit(decimal amount)
    {
        if (amount <= 0)
        {
            return LoanValidationResult.Error("Credit limit must be greater than zero");
        }

        if (amount > _maxCobolAmount)
        {
            return LoanValidationResult.Error("Credit limit exceeds maximum allowed value");
        }

        return LoanValidationResult.Success();
    }

    /// <summary>
    /// Validates a transaction amount (charge or repayment).
    /// COBOL: DALYTRAN-AMT — non-zero required.
    /// Business rule: LND-BR-005.
    /// </summary>
    public static LoanValidationResult ValidateTransactionAmount(decimal amount)
    {
        if (amount == 0)
        {
            return LoanValidationResult.Error("Transaction amount cannot be zero");
        }

        return LoanValidationResult.Success();
    }

    /// <summary>
    /// Validates a loan status transition.
    /// Business rule: LND-BR-008.
    /// Valid transitions:
    ///   Active → Delinquent, Frozen, PaidOff, Closed
    ///   Delinquent → Active, Defaulted, Frozen
    ///   Defaulted → Closed
    ///   Frozen → Active, Delinquent, Closed
    ///   PaidOff → Closed
    ///   Closed → (terminal — no transitions allowed)
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Inkassolagen 1974:182.
    /// </summary>
    public static LoanValidationResult ValidateStatusTransition(LoanStatus current, LoanStatus target)
    {
        if (current == target)
        {
            return LoanValidationResult.Error("Loan is already in the requested status");
        }

        if (current == LoanStatus.Closed)
        {
            return LoanValidationResult.Error("Cannot transition from Closed status — terminal state");
        }

        var validTransitions = GetValidTransitions(current);
        if (!validTransitions.Contains(target))
        {
            return LoanValidationResult.Error(
                $"Invalid transition from {current} to {target}");
        }

        return LoanValidationResult.Success();
    }

    private static LoanStatus[] GetValidTransitions(LoanStatus current) => current switch
    {
        LoanStatus.Active => [LoanStatus.Delinquent, LoanStatus.Frozen, LoanStatus.PaidOff, LoanStatus.Closed],
        LoanStatus.Delinquent => [LoanStatus.Active, LoanStatus.Defaulted, LoanStatus.Frozen],
        LoanStatus.Defaulted => [LoanStatus.Closed],
        LoanStatus.Frozen => [LoanStatus.Active, LoanStatus.Delinquent, LoanStatus.Closed],
        LoanStatus.PaidOff => [LoanStatus.Closed],
        LoanStatus.Closed => [],
        _ => throw new ArgumentOutOfRangeException(nameof(current), current, "Unknown loan status"),
    };

    /// <summary>
    /// Checks if a value is exactly the expected length and all ASCII digits, and not all zeros.
    /// COBOL: IS NUMERIC check + length validation + zeros check.
    /// </summary>
    private static bool IsValidNumericField(string value, int expectedLength) =>
        value.Length == expectedLength
        && value.All(char.IsAsciiDigit)
        && !value.All(c => c == '0');
}
