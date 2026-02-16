using Microsoft.EntityFrameworkCore;
using NordKredit.Infrastructure;
using NordKredit.Infrastructure.CardManagement;
using TransactionXref = NordKredit.Domain.Transactions.CardCrossReference;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for SqlCardCrossReferenceRepository (Card Management domain).
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), VSAM CARDXREF.
/// Regulations: GDPR Art. 5(1)(c), AML/KYC.
/// </summary>
public class SqlCardCrossReferenceRepositoryTests : IDisposable
{
    private readonly NordKreditDbContext _dbContext;
    private readonly SqlCardCrossReferenceRepository _repository;

    public SqlCardCrossReferenceRepositoryTests()
    {
        var options = new DbContextOptionsBuilder<NordKreditDbContext>()
            .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
            .Options;

        _dbContext = new NordKreditDbContext(options);
        _repository = new SqlCardCrossReferenceRepository(_dbContext);
    }

    public void Dispose()
    {
        GC.SuppressFinalize(this);
        _dbContext.Dispose();
    }

    private async Task SeedXrefAsync(string cardNumber, int customerId, string accountId)
    {
        _dbContext.CardCrossReferences.Add(new TransactionXref
        {
            CardNumber = cardNumber,
            CustomerId = customerId,
            AccountId = accountId
        });
        await _dbContext.SaveChangesAsync();
    }

    // --- GetByCardNumberAsync ---

    [Fact]
    public async Task GetByCardNumberAsync_ExistingXref_ReturnsRecord()
    {
        // CARD-BR-010 Scenario 1: Card-to-customer lookup
        await SeedXrefAsync("4000123456789012", 123456789, "12345678901");

        var result = await _repository.GetByCardNumberAsync("4000123456789012");

        Assert.NotNull(result);
        Assert.Equal("4000123456789012", result.CardNumber);
        Assert.Equal(123456789, result.CustomerId);
        Assert.Equal("12345678901", result.AccountId);
    }

    [Fact]
    public async Task GetByCardNumberAsync_NonExistent_ReturnsNull()
    {
        var result = await _repository.GetByCardNumberAsync("9999999999999999");

        Assert.Null(result);
    }

    // --- GetByCustomerIdAsync ---

    [Fact]
    public async Task GetByCustomerIdAsync_MultipleCards_ReturnsAll()
    {
        // CARD-BR-010 Scenario 2: Customer has multiple cards
        await SeedXrefAsync("4000123456789012", 123456789, "12345678901");
        await SeedXrefAsync("4000123456789028", 123456789, "12345678901");

        var result = await _repository.GetByCustomerIdAsync(123456789);

        Assert.Equal(2, result.Count);
    }

    [Fact]
    public async Task GetByCustomerIdAsync_CardsAcrossAccounts_ReturnsAll()
    {
        // CARD-BR-010 Scenario 3: Customer has cards on different accounts
        await SeedXrefAsync("4000123456789012", 123456789, "12345678901");
        await SeedXrefAsync("4000987654321098", 123456789, "98765432101");

        var result = await _repository.GetByCustomerIdAsync(123456789);

        Assert.Equal(2, result.Count);
        Assert.Contains(result, x => x.AccountId == "12345678901");
        Assert.Contains(result, x => x.AccountId == "98765432101");
    }

    [Fact]
    public async Task GetByCustomerIdAsync_OrderedByCardNumber()
    {
        await SeedXrefAsync("4000999999999999", 123456789, "12345678901");
        await SeedXrefAsync("4000111111111111", 123456789, "12345678901");

        var result = await _repository.GetByCustomerIdAsync(123456789);

        Assert.Equal("4000111111111111", result[0].CardNumber);
        Assert.Equal("4000999999999999", result[1].CardNumber);
    }

    [Fact]
    public async Task GetByCustomerIdAsync_NoCards_ReturnsEmpty()
    {
        var result = await _repository.GetByCustomerIdAsync(999999999);

        Assert.Empty(result);
    }

    [Fact]
    public async Task GetByCustomerIdAsync_OnlyReturnsMatchingCustomer()
    {
        // AML/KYC: Correct customer-to-card linkage
        await SeedXrefAsync("4000123456789012", 123456789, "12345678901");
        await SeedXrefAsync("4000999999999999", 987654321, "98765432101");

        var result = await _repository.GetByCustomerIdAsync(123456789);

        Assert.Single(result);
        Assert.Equal("4000123456789012", result[0].CardNumber);
    }
}
