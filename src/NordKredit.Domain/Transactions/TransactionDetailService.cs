namespace NordKredit.Domain.Transactions;

/// <summary>
/// Retrieves and formats a single transaction for read-only detail view.
/// COBOL source: COTRN01C.cbl:85-296 (CICS transaction CT01).
/// Replaces READ TRANSACT-FILE INTO WS-TRAN-RECORD (line 275).
/// Note: COBOL used READ with UPDATE lock — this is a legacy pattern; read-only is correct.
/// Regulations: FFFS 2014:5 Ch.8, PSD2 Art.94, GDPR Art.15.
/// </summary>
public class TransactionDetailService
{
    private readonly ITransactionRepository _transactionRepository;

    public TransactionDetailService(ITransactionRepository transactionRepository)
    {
        _transactionRepository = transactionRepository;
    }

    public async Task<TransactionDetailResponse?> GetByIdAsync(
        string transactionId,
        CancellationToken cancellationToken = default)
    {
        var transaction = await _transactionRepository.GetByIdAsync(transactionId, cancellationToken);

        if (transaction is null)
        {
            return null;
        }

        return new TransactionDetailResponse
        {
            TransactionId = transaction.Id,
            CardNumber = MaskCardNumber(transaction.CardNumber),
            TypeCode = transaction.TypeCode,
            CategoryCode = transaction.CategoryCode,
            Source = transaction.Source,
            Amount = transaction.Amount,
            Description = transaction.Description,
            OriginationTimestamp = transaction.OriginationTimestamp,
            ProcessingTimestamp = transaction.ProcessingTimestamp,
            MerchantId = transaction.MerchantId,
            MerchantName = transaction.MerchantName,
            MerchantCity = transaction.MerchantCity,
            MerchantZip = transaction.MerchantZip
        };
    }

    /// <summary>
    /// Masks card number for PCI-DSS compliance — show only last 4 digits.
    /// Cards shorter than 5 characters are returned as-is (not enough digits to mask).
    /// </summary>
    internal static string MaskCardNumber(string cardNumber)
    {
        if (string.IsNullOrEmpty(cardNumber) || cardNumber.Length <= 4)
        {
            return cardNumber;
        }

        return new string('*', cardNumber.Length - 4) + cardNumber[^4..];
    }
}
