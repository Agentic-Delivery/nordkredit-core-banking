namespace NordKredit.Domain.Transactions;

/// <summary>
/// Thrown when a transaction with a duplicate ID is inserted.
/// COBOL source: COTRN02C.cbl:735-738 — DFHRESP(DUPKEY)/DFHRESP(DUPREC).
/// </summary>
public class DuplicateTransactionException : Exception
{
    public string TransactionId { get; }

    public DuplicateTransactionException(string transactionId)
        : base($"Tran ID already exist...")
    {
        TransactionId = transactionId;
    }

    public DuplicateTransactionException(string transactionId, Exception innerException)
        : base($"Tran ID already exist...", innerException)
    {
        TransactionId = transactionId;
    }
}
