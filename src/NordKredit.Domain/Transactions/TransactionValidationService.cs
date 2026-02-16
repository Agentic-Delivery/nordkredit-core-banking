using System.Globalization;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Validates transaction add requests before creation.
/// COBOL source: COTRN02C.cbl:164-437 (VALIDATE-INPUT-KEY-FIELDS, VALIDATE-INPUT-DATA-FIELDS).
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (operational risk), AML/KYC (data completeness).
/// Validation exits on first error, matching COBOL sequential validation behavior.
/// </summary>
public class TransactionValidationService
{
    private readonly ICardCrossReferenceRepository _crossReferenceRepository;

    public TransactionValidationService(ICardCrossReferenceRepository crossReferenceRepository)
    {
        _crossReferenceRepository = crossReferenceRepository;
    }

    /// <summary>
    /// Validates a transaction add request following the COBOL sequential validation chain.
    /// COBOL source: COTRN02C.cbl:164-437.
    /// </summary>
    public async Task<TransactionValidationResult> ValidateAsync(
        TransactionAddRequest request,
        CancellationToken cancellationToken = default) =>
        await ValidateKeyFieldsAsync(request, cancellationToken);

    /// <summary>
    /// Validates key fields, resolves cross-reference, then validates data fields and confirmation.
    /// COBOL: VALIDATE-INPUT-KEY-FIELDS (COTRN02C.cbl:193-230).
    /// </summary>
    private async Task<TransactionValidationResult> ValidateKeyFieldsAsync(
        TransactionAddRequest request,
        CancellationToken cancellationToken)
    {
        if (!string.IsNullOrWhiteSpace(request.AccountId))
        {
            // Account ID provided — validate and look up card number
            if (!IsNumeric(request.AccountId))
            {
                return TransactionValidationResult.Error("Account ID must be Numeric...");
            }

            var xref = await _crossReferenceRepository.GetByAccountIdAsync(request.AccountId, cancellationToken);
            if (xref is null)
            {
                return TransactionValidationResult.Error("Account ID NOT found...");
            }

            // Continue to data field validation with resolved values
            return ValidateDataFields(request) ?? ValidateConfirmation(request, xref.CardNumber, xref.AccountId);
        }

        if (!string.IsNullOrWhiteSpace(request.CardNumber))
        {
            // Card number provided — validate and look up account ID
            if (!IsNumeric(request.CardNumber))
            {
                return TransactionValidationResult.Error("Card Number must be Numeric...");
            }

            var xref = await _crossReferenceRepository.GetByCardNumberAsync(request.CardNumber, cancellationToken);
            return xref is null
                ? TransactionValidationResult.Error("Card Number NOT found...")
                : ValidateDataFields(request) ?? ValidateConfirmation(request, xref.CardNumber, xref.AccountId);
        }

        // Neither provided
        return TransactionValidationResult.Error("Account or Card Number must be entered...");
    }

    /// <summary>
    /// Validates mandatory data fields and data types.
    /// COBOL: VALIDATE-INPUT-DATA-FIELDS (COTRN02C.cbl:235-427).
    /// Returns null if all data fields are valid; error result if not.
    /// </summary>
    private static TransactionValidationResult? ValidateDataFields(TransactionAddRequest request)
    {
        // Mandatory field checks — sequential, exit on first error
        // COBOL: lines 251-337
        if (string.IsNullOrWhiteSpace(request.TypeCode))
        {
            return TransactionValidationResult.Error("Type CD can NOT be empty...");
        }

        if (!IsNumeric(request.TypeCode))
        {
            return TransactionValidationResult.Error("Type CD must be Numeric...");
        }

        if (string.IsNullOrWhiteSpace(request.CategoryCode))
        {
            return TransactionValidationResult.Error("Category CD can NOT be empty...");
        }

        if (!IsNumeric(request.CategoryCode))
        {
            return TransactionValidationResult.Error("Category CD must be Numeric...");
        }

        if (string.IsNullOrWhiteSpace(request.Source))
        {
            return TransactionValidationResult.Error("Source can NOT be empty...");
        }

        if (string.IsNullOrWhiteSpace(request.Description))
        {
            return TransactionValidationResult.Error("Description can NOT be empty...");
        }

        if (string.IsNullOrWhiteSpace(request.Amount))
        {
            return TransactionValidationResult.Error("Amount can NOT be empty...");
        }

        // Amount format: position 1 = +/-, positions 2-9 = numeric, position 10 = '.', positions 11-12 = numeric
        // COBOL: lines 339-345
        if (!IsValidAmountFormat(request.Amount))
        {
            return TransactionValidationResult.Error("Amount should be in format -99999999.99");
        }

        if (string.IsNullOrWhiteSpace(request.OriginationDate))
        {
            return TransactionValidationResult.Error("Orig Date can NOT be empty...");
        }

        // Date format and validity — replaces CSUTLDTC utility
        // COBOL: lines 347-413
        if (!IsValidDate(request.OriginationDate))
        {
            return TransactionValidationResult.Error("Orig Date - Not a valid date...");
        }

        if (string.IsNullOrWhiteSpace(request.ProcessingDate))
        {
            return TransactionValidationResult.Error("Proc Date can NOT be empty...");
        }

        if (!IsValidDate(request.ProcessingDate))
        {
            return TransactionValidationResult.Error("Proc Date - Not a valid date...");
        }

        if (string.IsNullOrWhiteSpace(request.MerchantId))
        {
            return TransactionValidationResult.Error("Merchant ID can NOT be empty...");
        }

        if (!IsNumeric(request.MerchantId))
        {
            return TransactionValidationResult.Error("Merchant ID must be Numeric...");
        }

        if (string.IsNullOrWhiteSpace(request.MerchantName))
        {
            return TransactionValidationResult.Error("Merchant Name can NOT be empty...");
        }

        if (string.IsNullOrWhiteSpace(request.MerchantCity))
        {
            return TransactionValidationResult.Error("Merchant City can NOT be empty...");
        }

        if (string.IsNullOrWhiteSpace(request.MerchantZip))
        {
            return TransactionValidationResult.Error("Merchant Zip can NOT be empty...");
        }

        return null;
    }

    /// <summary>
    /// Validates the confirmation flag.
    /// COBOL: COTRN02C.cbl:429-437.
    /// </summary>
    private static TransactionValidationResult ValidateConfirmation(
        TransactionAddRequest request,
        string resolvedCardNumber,
        string resolvedAccountId) =>
        request.Confirm.ToUpperInvariant() switch
        {
            "Y" => TransactionValidationResult.Success(resolvedCardNumber, resolvedAccountId),
            "N" or "" => TransactionValidationResult.NeedsConfirmation(resolvedCardNumber, resolvedAccountId),
            _ => TransactionValidationResult.Error("Invalid value. Valid values are (Y/N)...")
        };

    /// <summary>
    /// Checks if all characters in the string are digits (COBOL IS NUMERIC check).
    /// </summary>
    private static bool IsNumeric(string value) =>
        value.All(char.IsAsciiDigit);

    /// <summary>
    /// Validates amount format: ±99999999.99
    /// Position 1: +/-, positions 2-9: numeric, position 10: '.', positions 11-12: numeric.
    /// COBOL: COTRN02C.cbl:339-345.
    /// </summary>
    private static bool IsValidAmountFormat(string amount)
    {
        if (amount.Length != 12)
        {
            return false;
        }

        if (amount[0] is not '+' and not '-')
        {
            return false;
        }

        for (var i = 1; i <= 8; i++)
        {
            if (!char.IsAsciiDigit(amount[i]))
            {
                return false;
            }
        }

        return amount[9] == '.' && char.IsAsciiDigit(amount[10]) && char.IsAsciiDigit(amount[11]);
    }

    /// <summary>
    /// Validates date string is in YYYY-MM-DD format and is a valid calendar date.
    /// Replaces CSUTLDTC date validation utility.
    /// COBOL: COTRN02C.cbl:347-427.
    /// </summary>
    private static bool IsValidDate(string date) =>
        DateTime.TryParseExact(date, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out _);
}
