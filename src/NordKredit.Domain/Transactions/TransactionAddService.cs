using System.Globalization;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Orchestrates adding a new transaction: validate, generate ID, map fields, persist.
/// COBOL source: COTRN02C.cbl:442-749 (ADD-TRANSACTION, WRITE-TRANSACT-FILE).
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention).
/// </summary>
public class TransactionAddService
{
    private readonly TransactionValidationService _validationService;
    private readonly ITransactionIdGenerator _idGenerator;
    private readonly ITransactionRepository _transactionRepository;

    public TransactionAddService(
        TransactionValidationService validationService,
        ITransactionIdGenerator idGenerator,
        ITransactionRepository transactionRepository)
    {
        _validationService = validationService;
        _idGenerator = idGenerator;
        _transactionRepository = transactionRepository;
    }

    /// <summary>
    /// Adds a new transaction after validation and ID generation.
    /// COBOL: PROCESS-ENTER-KEY → VALIDATE-INPUT → ADD-TRANSACTION → WRITE-TRANSACT-FILE.
    /// </summary>
    public async Task<TransactionAddResult> AddTransactionAsync(
        TransactionAddRequest request,
        CancellationToken cancellationToken = default)
    {
        // Step 1: Validate input (COTRN02C.cbl:164-437)
        var validationResult = await _validationService.ValidateAsync(request, cancellationToken);

        if (validationResult.ConfirmationRequired)
        {
            return TransactionAddResult.NeedsConfirmation();
        }

        if (!validationResult.IsValid)
        {
            return TransactionAddResult.ValidationError(validationResult.ErrorMessage!);
        }

        // Step 2: Generate transaction ID (COTRN02C.cbl:442-449)
        var transactionId = await _idGenerator.GenerateNextIdAsync(cancellationToken);

        // Step 3: Map request to transaction entity (COTRN02C.cbl:450-458)
        var transaction = MapToTransaction(request, transactionId, validationResult);

        // Step 4: Persist (COTRN02C.cbl:711-738)
        try
        {
            await _transactionRepository.AddAsync(transaction, cancellationToken);
            return TransactionAddResult.Success(transactionId);
        }
        catch (DuplicateTransactionException)
        {
            return TransactionAddResult.DuplicateKey();
        }
    }

    /// <summary>
    /// Retrieves the last (most recent) transaction for copy-last-transaction convenience.
    /// COBOL: COPY-LAST-TRAN-DATA (PF5 handler), COTRN02C.cbl.
    /// </summary>
    public async Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default) =>
        await _transactionRepository.GetLastTransactionAsync(cancellationToken);

    /// <summary>
    /// Maps the request DTO to a Transaction entity.
    /// COBOL: COTRN02C.cbl:450-458 — MOVE screen fields to TRAN-RECORD fields.
    /// Amount conversion replaces FUNCTION NUMVAL-C (COTRN02C.cbl:456).
    /// </summary>
    private static Transaction MapToTransaction(
        TransactionAddRequest request,
        string transactionId,
        TransactionValidationResult validationResult)
    {
        // Amount conversion: replaces COBOL FUNCTION NUMVAL-C
        // Format is already validated as ±99999999.99 by TransactionValidationService
        var amount = decimal.Parse(request.Amount, CultureInfo.InvariantCulture);

        return new Transaction
        {
            Id = transactionId,
            TypeCode = request.TypeCode,
            CategoryCode = int.Parse(request.CategoryCode, CultureInfo.InvariantCulture),
            Source = request.Source,
            Description = request.Description,
            Amount = amount,
            MerchantId = int.Parse(request.MerchantId, CultureInfo.InvariantCulture),
            MerchantName = request.MerchantName,
            MerchantCity = request.MerchantCity,
            MerchantZip = request.MerchantZip,
            CardNumber = validationResult.ResolvedCardNumber!,
            OriginationTimestamp = DateTime.ParseExact(
                request.OriginationDate, "yyyy-MM-dd", CultureInfo.InvariantCulture),
            ProcessingTimestamp = DateTime.ParseExact(
                request.ProcessingDate, "yyyy-MM-dd", CultureInfo.InvariantCulture)
        };
    }
}
