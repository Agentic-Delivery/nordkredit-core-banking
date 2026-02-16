namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Validates card update fields: embossed name, active status, expiry month, and expiry year.
/// Also detects when no modifications were made (case-insensitive comparison).
/// COBOL source: COCRDUPC.cbl:806-947, 87-99, 255-263.
/// Business rule: CARD-BR-006.
/// Regulations: FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97, GDPR Art. 5(1)(d).
/// </summary>
public static class CardUpdateValidator
{
    /// <summary>
    /// Validates the embossed name field.
    /// COBOL: paragraph 1230-EDIT-NAME (COCRDUPC.cbl:806-843).
    /// Must contain only Unicode letters and spaces. Extends COBOL A-Z/a-z to include
    /// Swedish characters (Å, Ä, Ö) and other Unicode letter categories.
    /// </summary>
    public static CardValidationResult ValidateEmbossedName(string? embossedName)
    {
        // Blank check — COBOL: IF CCUP-NEW-CRDNAME = LOW-VALUES OR SPACES OR ZEROS
        if (string.IsNullOrWhiteSpace(embossedName))
        {
            return CardValidationResult.Error("Card name not provided");
        }

        // Alpha check — COBOL: INSPECT CARD-NAME-CHECK CONVERTING LIT-ALL-ALPHA-FROM TO LIT-ALL-SPACES-TO
        // Extended from COBOL A-Z/a-z (PIC X(52)) to Unicode letters for Swedish names (Å, Ä, Ö).
        foreach (var c in embossedName)
        {
            if (!char.IsLetter(c) && c != ' ')
            {
                return CardValidationResult.Error("Card name can only contain alphabets and spaces");
            }
        }

        return CardValidationResult.Success();
    }

    /// <summary>
    /// Validates the active status field.
    /// COBOL: paragraph 1240-EDIT-CARDSTATUS (COCRDUPC.cbl:845-876).
    /// Must be 'Y' or 'N' (uppercase only, per COBOL 88-level FLG-YES-NO-VALID).
    /// </summary>
    public static CardValidationResult ValidateActiveStatus(char activeStatus)
    {
        // COBOL: 88 FLG-YES-NO-VALID VALUES 'Y', 'N'.
        if (activeStatus is 'Y' or 'N')
        {
            return CardValidationResult.Success();
        }

        return CardValidationResult.Error("Card Active Status must be Y or N");
    }

    /// <summary>
    /// Validates the expiry month field.
    /// COBOL: paragraph 1250-EDIT-EXPIRY-MON (COCRDUPC.cbl:877-912).
    /// Must be between 1 and 12 inclusive. COBOL: 88 VALID-MONTH VALUES 1 THRU 12.
    /// </summary>
    public static CardValidationResult ValidateExpiryMonth(int expiryMonth)
    {
        // COBOL: 88 VALID-MONTH VALUES 1 THRU 12.
        if (expiryMonth is >= 1 and <= 12)
        {
            return CardValidationResult.Success();
        }

        return CardValidationResult.Error("Card expiry month must be between 1 and 12");
    }

    /// <summary>
    /// Validates the expiry year field.
    /// COBOL: paragraph 1260-EDIT-EXPIRY-YEAR (COCRDUPC.cbl:913-947).
    /// Must be between 1950 and 2099 inclusive. COBOL: 88 VALID-YEAR VALUES 1950 THRU 2099.
    /// </summary>
    public static CardValidationResult ValidateExpiryYear(int expiryYear)
    {
        // COBOL: 88 VALID-YEAR VALUES 1950 THRU 2099.
        if (expiryYear is >= 1950 and <= 2099)
        {
            return CardValidationResult.Success();
        }

        return CardValidationResult.Error("Invalid card expiry year");
    }

    /// <summary>
    /// Validates all update fields and returns ALL errors simultaneously.
    /// Improvement over COBOL: COCRDUPC.cbl only reported the first error via WS-RETURN-MSG-OFF flag.
    /// The migrated system collects every validation failure and returns them together.
    /// </summary>
    public static CardUpdateValidationResult ValidateUpdate(
        string? embossedName,
        char activeStatus,
        int expiryMonth,
        int expiryYear)
    {
        var errors = new List<string>();

        var nameResult = ValidateEmbossedName(embossedName);
        if (!nameResult.IsValid)
        {
            errors.Add(nameResult.ErrorMessage!);
        }

        var statusResult = ValidateActiveStatus(activeStatus);
        if (!statusResult.IsValid)
        {
            errors.Add(statusResult.ErrorMessage!);
        }

        var monthResult = ValidateExpiryMonth(expiryMonth);
        if (!monthResult.IsValid)
        {
            errors.Add(monthResult.ErrorMessage!);
        }

        var yearResult = ValidateExpiryYear(expiryYear);
        if (!yearResult.IsValid)
        {
            errors.Add(yearResult.ErrorMessage!);
        }

        return errors.Count == 0
            ? CardUpdateValidationResult.Success()
            : CardUpdateValidationResult.WithErrors(errors);
    }

    /// <summary>
    /// Detects whether new values differ from the existing card record.
    /// Uses case-insensitive comparison for embossed name (COBOL: UPPER-CASE function).
    /// Expiry day is NOT user-editable and is carried from the original record.
    /// COBOL source: COCRDUPC.cbl 1200-EDIT-MAP-INPUTS.
    /// </summary>
    public static CardChangeDetectionResult DetectChanges(
        Card existing,
        string newEmbossedName,
        char newActiveStatus,
        int newExpiryMonth,
        int newExpiryYear)
    {
        // COBOL: COMPARE new-card-data WITH old-card-data (case-insensitive via UPPER-CASE)
        var nameChanged = !string.Equals(
            existing.EmbossedName,
            newEmbossedName,
            StringComparison.OrdinalIgnoreCase);

        var statusChanged = existing.ActiveStatus != newActiveStatus;

        var monthChanged = existing.ExpirationDate.Month != newExpiryMonth;

        var yearChanged = existing.ExpirationDate.Year != newExpiryYear;

        if (nameChanged || statusChanged || monthChanged || yearChanged)
        {
            return CardChangeDetectionResult.Changed();
        }

        return CardChangeDetectionResult.NoChange();
    }
}
