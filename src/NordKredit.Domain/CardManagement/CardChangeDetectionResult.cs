namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Result of comparing new card data with existing card data.
/// COBOL source: COCRDUPC.cbl 1200-EDIT-MAP-INPUTS — case-insensitive comparison via UPPER-CASE.
/// Regulations: GDPR Art. 5(1)(d) — prevents unnecessary data modifications.
/// </summary>
public class CardChangeDetectionResult
{
    /// <summary>Whether any field value differs from the stored record.</summary>
    public bool HasChanges { get; }

    /// <summary>
    /// Message when no changes detected. Maps to COBOL WS-MESSAGE.
    /// Null when changes are detected.
    /// </summary>
    public string? Message { get; }

    private CardChangeDetectionResult(bool hasChanges, string? message)
    {
        HasChanges = hasChanges;
        Message = message;
    }

    public static CardChangeDetectionResult Changed()
        => new(true, null);

    public static CardChangeDetectionResult NoChange()
        => new(false, "No change detected with respect to values fetched.");
}
