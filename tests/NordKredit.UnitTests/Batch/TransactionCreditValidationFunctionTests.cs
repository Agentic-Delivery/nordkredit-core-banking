using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using NordKredit.Functions.Batch;
using NordKredit.UnitTests.Transactions;

namespace NordKredit.UnitTests.Batch;

/// <summary>
/// Unit tests for TransactionCreditValidationFunction — batch Azure Function wrapper.
/// COBOL source: CBTRN02C.cbl:370-422.
/// Verifies function-level orchestration, result aggregation, and warning status.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public class TransactionCreditValidationFunctionTests
{
    private readonly StubCardCrossReferenceRepository2 _crossRefRepo = new();
    private readonly StubAccountRepository2 _accountRepo = new();
    private readonly StubDailyRejectRepository _rejectRepo = new();
    private readonly ILogger<TransactionCreditValidationService> _serviceLogger =
        NullLogger<TransactionCreditValidationService>.Instance;
    private readonly ILogger<TransactionCreditValidationFunction> _functionLogger =
        NullLogger<TransactionCreditValidationFunction>.Instance;

    private TransactionCreditValidationFunction CreateFunction()
    {
        var service = new TransactionCreditValidationService(
            _crossRefRepo,
            _accountRepo,
            _rejectRepo,
            _serviceLogger);
        return new TransactionCreditValidationFunction(service, _functionLogger);
    }

    // ===================================================================
    // AC-1: Valid transaction — passes credit limit and expiration
    // ===================================================================

    [Fact]
    public async Task RunAsync_ValidTransaction_ReturnsValidResult()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 1000.00m,
            CurrentCycleDebit = 500.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN001", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        var function = CreateFunction();
        var result = await function.RunAsync([verified]);

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(1, result.ValidCount);
        Assert.Equal(0, result.RejectedCount);
        Assert.False(result.HasWarnings);
    }

    // ===================================================================
    // AC-2: Overlimit transaction — REJECTED, HasWarnings = true
    // ===================================================================

    [Fact]
    public async Task RunAsync_OverlimitTransaction_ReturnsRejectedWithWarnings()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 4000.00m,
            CurrentCycleDebit = 200.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN002", "4000000000000001",
            "00000000001", 100000001, 1500.00m, new DateTime(2026, 1, 15));

        var function = CreateFunction();
        var result = await function.RunAsync([verified]);

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.ValidCount);
        Assert.Equal(1, result.RejectedCount);
        Assert.True(result.HasWarnings);
    }

    // ===================================================================
    // AC-3: Empty input — no transactions
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoTransactions_ReturnsEmptyResult()
    {
        var function = CreateFunction();
        var result = await function.RunAsync([]);

        Assert.Equal(0, result.TotalProcessed);
        Assert.Equal(0, result.ValidCount);
        Assert.Equal(0, result.RejectedCount);
        Assert.False(result.HasWarnings);
        Assert.Empty(result.Results);
    }

    // ===================================================================
    // AC-4: Mixed outcomes — multiple transactions
    // ===================================================================

    [Fact]
    public async Task RunAsync_MixedTransactions_ReturnsCorrectCounts()
    {
        var account1 = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 1000.00m,
            CurrentCycleDebit = 500.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account1);

        // Valid transaction
        var v1 = CreateVerifiedTransaction("TXN010", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        // Invalid card
        var txn2 = CreateDailyTransaction("TXN011", "9999999999999999", 100.00m, new DateTime(2026, 1, 15));
        var v2 = new VerifiedTransaction
        {
            Transaction = txn2,
            IsVerified = false,
            FailureReason = "Card number could not be verified",
            AccountId = null,
            CustomerId = null
        };

        var function = CreateFunction();
        var result = await function.RunAsync([v1, v2]);

        Assert.Equal(2, result.TotalProcessed);
        Assert.Equal(1, result.ValidCount);
        Assert.Equal(1, result.RejectedCount);
        Assert.True(result.HasWarnings);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        var verified = CreateVerifiedTransaction("TXN020", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync([verified], cts.Token));
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static VerifiedTransaction CreateVerifiedTransaction(
        string id, string cardNumber, string accountId, int customerId,
        decimal amount, DateTime originationDate)
    {
        var transaction = CreateDailyTransaction(id, cardNumber, amount, originationDate);
        return new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = true,
            AccountId = accountId,
            CustomerId = customerId
        };
    }

    private static DailyTransaction CreateDailyTransaction(
        string id, string cardNumber, decimal amount, DateTime originationDate) => new()
        {
            Id = id,
            CardNumber = cardNumber,
            TypeCode = "01",
            CategoryCode = 1001,
            Source = "BATCH",
            Description = "Test daily transaction",
            Amount = amount,
            MerchantId = 1,
            MerchantName = "Test Merchant",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = originationDate,
            ProcessingTimestamp = originationDate.AddDays(1)
        };
}
