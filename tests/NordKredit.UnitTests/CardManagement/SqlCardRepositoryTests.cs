using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.CardManagement;
using NordKredit.Infrastructure;
using NordKredit.Infrastructure.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for SqlCardRepository — Azure SQL implementation of ICardRepository.
/// COBOL source: VSAM CARDDAT (primary key), CARDAIX (alternate index).
/// </summary>
public class SqlCardRepositoryTests : IDisposable
{
    private readonly NordKreditDbContext _dbContext;
    private readonly SqlCardRepository _repository;

    public SqlCardRepositoryTests()
    {
        var options = new DbContextOptionsBuilder<NordKreditDbContext>()
            .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
            .Options;

        _dbContext = new NordKreditDbContext(options);
        _repository = new SqlCardRepository(_dbContext);
    }

    public void Dispose()
    {
        GC.SuppressFinalize(this);
        _dbContext.Dispose();
    }

    private static Card CreateCard(
        string cardNumber = "4000123456789012",
        string accountId = "12345678901",
        char activeStatus = 'Y')
    {
        return new Card
        {
            CardNumber = cardNumber,
            AccountId = accountId,
            CvvCode = "123",
            EmbossedName = "JOHN DOE",
            ExpirationDate = new DateOnly(2027, 12, 31),
            ActiveStatus = activeStatus
        };
    }

    private async Task SeedCardAsync(Card card)
    {
        _dbContext.Cards.Add(card);
        await _dbContext.SaveChangesAsync();
        _dbContext.ChangeTracker.Clear();
    }

    // --- GetByCardNumberAsync ---

    [Fact]
    public async Task GetByCardNumberAsync_ExistingCard_ReturnsCard()
    {
        // COBOL: VSAM READ on CARDDAT by primary key
        var card = CreateCard();
        await SeedCardAsync(card);

        var result = await _repository.GetByCardNumberAsync("4000123456789012");

        Assert.NotNull(result);
        Assert.Equal("4000123456789012", result.CardNumber);
        Assert.Equal("12345678901", result.AccountId);
    }

    [Fact]
    public async Task GetByCardNumberAsync_NonExistentCard_ReturnsNull()
    {
        var result = await _repository.GetByCardNumberAsync("9999999999999999");

        Assert.Null(result);
    }

    // --- GetByAccountIdAsync ---

    [Fact]
    public async Task GetByAccountIdAsync_MultipleCards_ReturnsAll()
    {
        // COBOL: VSAM browse on CARDAIX alternate index
        await SeedCardAsync(CreateCard("4000123456789012", "12345678901"));
        await SeedCardAsync(CreateCard("4000123456789028", "12345678901"));

        var result = await _repository.GetByAccountIdAsync("12345678901");

        Assert.Equal(2, result.Count);
    }

    [Fact]
    public async Task GetByAccountIdAsync_OrderedByCardNumber()
    {
        await SeedCardAsync(CreateCard("4000999999999999", "12345678901"));
        await SeedCardAsync(CreateCard("4000111111111111", "12345678901"));

        var result = await _repository.GetByAccountIdAsync("12345678901");

        Assert.Equal("4000111111111111", result[0].CardNumber);
        Assert.Equal("4000999999999999", result[1].CardNumber);
    }

    [Fact]
    public async Task GetByAccountIdAsync_NoCards_ReturnsEmpty()
    {
        var result = await _repository.GetByAccountIdAsync("99999999999");

        Assert.Empty(result);
    }

    // --- GetPageForwardAsync ---

    [Fact]
    public async Task GetPageForwardAsync_ReturnsPageSizeCards()
    {
        // COBOL: COCRDLIC.cbl — PROCESS-PAGE-FORWARD (STARTBR + READNEXT)
        await SeedCardAsync(CreateCard("4000000000000001"));
        await SeedCardAsync(CreateCard("4000000000000002"));
        await SeedCardAsync(CreateCard("4000000000000003"));

        var result = await _repository.GetPageForwardAsync(2);

        Assert.Equal(2, result.Count);
        Assert.Equal("4000000000000001", result[0].CardNumber);
        Assert.Equal("4000000000000002", result[1].CardNumber);
    }

    [Fact]
    public async Task GetPageForwardAsync_WithCursor_StartsAfterCursor()
    {
        await SeedCardAsync(CreateCard("4000000000000001"));
        await SeedCardAsync(CreateCard("4000000000000002"));
        await SeedCardAsync(CreateCard("4000000000000003"));

        var result = await _repository.GetPageForwardAsync(10, "4000000000000001");

        Assert.Equal(2, result.Count);
        Assert.Equal("4000000000000002", result[0].CardNumber);
        Assert.Equal("4000000000000003", result[1].CardNumber);
    }

    [Fact]
    public async Task GetPageForwardAsync_NoCards_ReturnsEmpty()
    {
        var result = await _repository.GetPageForwardAsync(10);

        Assert.Empty(result);
    }

    // --- GetPageBackwardAsync ---

    [Fact]
    public async Task GetPageBackwardAsync_ReturnsDescendingOrder()
    {
        // COBOL: COCRDLIC.cbl — PROCESS-PAGE-BACKWARD (STARTBR + READPREV, PF7)
        await SeedCardAsync(CreateCard("4000000000000001"));
        await SeedCardAsync(CreateCard("4000000000000002"));
        await SeedCardAsync(CreateCard("4000000000000003"));

        var result = await _repository.GetPageBackwardAsync(2);

        Assert.Equal(2, result.Count);
        Assert.Equal("4000000000000003", result[0].CardNumber);
        Assert.Equal("4000000000000002", result[1].CardNumber);
    }

    [Fact]
    public async Task GetPageBackwardAsync_WithCursor_StartsBeforeCursor()
    {
        await SeedCardAsync(CreateCard("4000000000000001"));
        await SeedCardAsync(CreateCard("4000000000000002"));
        await SeedCardAsync(CreateCard("4000000000000003"));

        var result = await _repository.GetPageBackwardAsync(10, "4000000000000003");

        Assert.Equal(2, result.Count);
        Assert.Equal("4000000000000002", result[0].CardNumber);
        Assert.Equal("4000000000000001", result[1].CardNumber);
    }

    // --- UpdateAsync ---

    [Fact]
    public async Task UpdateAsync_ModifiesCardFields()
    {
        // COBOL: COCRDUPC.cbl — REWRITE CARD-RECORD
        var card = CreateCard();
        await SeedCardAsync(card);

        card.EmbossedName = "BJÖRK ÅSTRÖM";
        card.ActiveStatus = 'N';
        await _repository.UpdateAsync(card);

        var updated = await _repository.GetByCardNumberAsync("4000123456789012");
        Assert.NotNull(updated);
        Assert.Equal("BJÖRK ÅSTRÖM", updated.EmbossedName);
        Assert.Equal('N', updated.ActiveStatus);
    }

    [Fact]
    public async Task UpdateAsync_SwedishCharacters_Preserved()
    {
        // EBCDIC→UTF-8: Swedish characters (Å, Ä, Ö) must be preserved
        var card = CreateCard();
        card.EmbossedName = "ÅSA ÖBERG";
        await SeedCardAsync(card);

        var result = await _repository.GetByCardNumberAsync("4000123456789012");
        Assert.NotNull(result);
        Assert.Equal("ÅSA ÖBERG", result.EmbossedName);
    }
}
