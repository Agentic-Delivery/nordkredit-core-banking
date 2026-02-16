namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Result of a card update operation, representing the state machine outcomes.
/// COBOL source: COCRDUPC.cbl:275-290, 429-543, 948-1027 (state machine transitions).
/// Business rules: CARD-BR-007 (state machine), CARD-BR-008 (optimistic concurrency).
/// Regulations: PSD2 Art. 97, FFFS 2014:5 Ch. 8 §4.
/// </summary>
public class CardUpdateResult
{
    /// <summary>Whether the update completed successfully.</summary>
    public bool IsSuccess { get; }

    /// <summary>Whether no changes were detected (values identical to current record).</summary>
    public bool IsNoChange { get; }

    /// <summary>Whether a concurrency conflict was detected.</summary>
    public bool IsConflict { get; }

    /// <summary>Whether validation failed.</summary>
    public bool IsValidationFailure { get; }

    /// <summary>Whether the card was not found.</summary>
    public bool IsNotFound { get; }

    /// <summary>Whether the database write failed after lock acquisition.</summary>
    public bool IsWriteFailure { get; }

    /// <summary>Message describing the result. Maps to COBOL WS-MESSAGE.</summary>
    public string? Message { get; }

    /// <summary>Validation errors, if any.</summary>
    public IReadOnlyList<string> ValidationErrors { get; }

    /// <summary>Updated card data (for success, no-change, or conflict responses).</summary>
    public Card? Card { get; }

    private CardUpdateResult(
        bool isSuccess,
        bool isNoChange,
        bool isConflict,
        bool isValidationFailure,
        bool isNotFound,
        bool isWriteFailure,
        string? message,
        IReadOnlyList<string>? validationErrors,
        Card? card)
    {
        IsSuccess = isSuccess;
        IsNoChange = isNoChange;
        IsConflict = isConflict;
        IsValidationFailure = isValidationFailure;
        IsNotFound = isNotFound;
        IsWriteFailure = isWriteFailure;
        Message = message;
        ValidationErrors = validationErrors ?? [];
        Card = card;
    }

    /// <summary>COBOL: CHANGES-OKAYED-AND-DONE — update committed successfully.</summary>
    public static CardUpdateResult Success(Card card) =>
        new(true, false, false, false, false, false, null, null, card);

    /// <summary>COBOL: SHOW-DETAILS with no-change message — values identical to current record.</summary>
    public static CardUpdateResult NoChange(string message, Card card) =>
        new(false, true, false, false, false, false, message, null, card);

    /// <summary>COBOL: DATA-WAS-CHANGED-BEFORE-UPDATE — concurrency conflict detected.</summary>
    public static CardUpdateResult Conflict(string message, Card card) =>
        new(false, false, true, false, false, false, message, null, card);

    /// <summary>COBOL: CHANGES-NOT-OK — field validation errors.</summary>
    public static CardUpdateResult ValidationFailure(IReadOnlyList<string> errors) =>
        new(false, false, false, true, false, false, null, errors, null);

    /// <summary>Card not found in repository.</summary>
    public static CardUpdateResult NotFound() =>
        new(false, false, false, false, true, false, null, null, null);

    /// <summary>COBOL: CHANGES-OKAYED-BUT-FAILED — database write failed after lock.</summary>
    public static CardUpdateResult WriteFailure(string message) =>
        new(false, false, false, false, false, true, message, null, null);
}
