namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of a transaction add operation.
/// COBOL source: COTRN02C.cbl:723-738 — maps to WRITE response codes and WS-MESSAGE.
/// </summary>
public class TransactionAddResult
{
    /// <summary>Whether the transaction was successfully created.</summary>
    public bool IsSuccess { get; }

    /// <summary>Whether the user needs to confirm before proceeding.</summary>
    public bool ConfirmationRequired { get; }

    /// <summary>The generated Transaction ID (populated on success). COBOL: TRAN-ID PIC X(16).</summary>
    public string? TransactionId { get; }

    /// <summary>Message describing the outcome. Maps to COBOL WS-MESSAGE.</summary>
    public string Message { get; }

    private TransactionAddResult(bool isSuccess, bool confirmationRequired, string? transactionId, string message)
    {
        IsSuccess = isSuccess;
        ConfirmationRequired = confirmationRequired;
        TransactionId = transactionId;
        Message = message;
    }

    /// <summary>
    /// Transaction created successfully.
    /// COBOL: DFHRESP(NORMAL) — "Transaction added successfully. Your Tran ID is NNNN."
    /// </summary>
    public static TransactionAddResult Success(string transactionId)
        => new(true, false, transactionId,
            $"Transaction added successfully.  Your Tran ID is {transactionId}.");

    /// <summary>
    /// Validation failed.
    /// COBOL: WS-ERR-FLG = 'Y', WS-MESSAGE = error text.
    /// </summary>
    public static TransactionAddResult ValidationError(string errorMessage)
        => new(false, false, null, errorMessage);

    /// <summary>
    /// User needs to confirm the transaction (Confirm != 'Y').
    /// COBOL: PROCESS-ENTER-KEY when confirm is not 'Y'.
    /// </summary>
    public static TransactionAddResult NeedsConfirmation()
        => new(false, true, null, "Please confirm the transaction by setting Confirm to 'Y'.");

    /// <summary>
    /// Duplicate key — Transaction ID already exists.
    /// COBOL: DFHRESP(DUPKEY)/DFHRESP(DUPREC) — "Tran ID already exist..."
    /// </summary>
    public static TransactionAddResult DuplicateKey()
        => new(false, false, null, "Tran ID already exist...");

    /// <summary>
    /// Unexpected write failure.
    /// COBOL: Other RESP codes — "Unable to Add Transaction..."
    /// </summary>
    public static TransactionAddResult WriteError()
        => new(false, false, null, "Unable to Add Transaction...");
}
