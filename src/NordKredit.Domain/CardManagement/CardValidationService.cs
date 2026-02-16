namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Shared input validation for account numbers and card numbers used across card management programs.
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-724, COCRDUPC.cbl:721-800.
/// Consolidates identical validation logic duplicated in list (COCRDLIC), detail (COCRDSLC),
/// and update (COCRDUPC) programs into a single reusable service.
/// Regulations: FFFS 2014:5 Ch. 4 §3 (operational risk), PSD2 Art. 97.
/// </summary>
public static class CardValidationService
{
    /// <summary>
    /// Validates an account number input.
    /// COBOL: EDIT-ACCOUNT paragraph — COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760.
    /// Business rule: CARD-BR-004.
    /// </summary>
    /// <param name="accountNumber">The account number to validate.</param>
    /// <param name="required">
    /// True for detail/update programs (blank = error).
    /// False for list program (blank = skip filter).
    /// </param>
    public static CardValidationResult ValidateAccountNumber(string? accountNumber, bool required)
    {
        // Blank check — COBOL: IF account-id = SPACES OR LOW-VALUES
        if (string.IsNullOrWhiteSpace(accountNumber))
        {
            return required
                ? CardValidationResult.Error("Account number not provided")
                : CardValidationResult.Success();
        }

        // Numeric and length check — COBOL: IF account-id IS NOT NUMERIC
        // Also catches all-zeros — COBOL: IF account-id = ZEROS
        if (!IsValidNumericField(accountNumber, 11))
        {
            return CardValidationResult.Error("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER");
        }

        return CardValidationResult.Success();
    }

    /// <summary>
    /// Validates a card number input.
    /// COBOL: EDIT-CARD paragraph — COCRDSLC.cbl:685-724, COCRDUPC.cbl:762-800.
    /// Business rule: CARD-BR-005.
    /// </summary>
    /// <param name="cardNumber">The card number to validate.</param>
    /// <param name="required">
    /// True for detail/update programs (blank = error).
    /// False for list program (blank = skip filter).
    /// </param>
    public static CardValidationResult ValidateCardNumber(string? cardNumber, bool required)
    {
        // Blank check — COBOL: IF card-number = SPACES OR LOW-VALUES OR ZEROS
        // Card number treats all-zeros as blank (unlike account number)
        if (string.IsNullOrWhiteSpace(cardNumber) || IsAllZeros(cardNumber))
        {
            return required
                ? CardValidationResult.Error("Card number not provided")
                : CardValidationResult.Success();
        }

        // Numeric and length check — COBOL: IF card-number IS NOT NUMERIC
        if (!IsValidNumericField(cardNumber, 16))
        {
            return CardValidationResult.Error("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER");
        }

        return CardValidationResult.Success();
    }

    /// <summary>
    /// Checks if a value is exactly the expected length and all ASCII digits, and not all zeros.
    /// Combines the COBOL IS NUMERIC check with length validation and zeros check.
    /// </summary>
    private static bool IsValidNumericField(string value, int expectedLength) =>
        value.Length == expectedLength
        && value.All(char.IsAsciiDigit)
        && !IsAllZeros(value);

    /// <summary>
    /// Checks if the string consists entirely of '0' characters.
    /// COBOL: IF field = ZEROS.
    /// </summary>
    private static bool IsAllZeros(string value) =>
        value.All(c => c == '0');
}
