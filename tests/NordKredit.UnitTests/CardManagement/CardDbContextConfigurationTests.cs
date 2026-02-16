using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.CardManagement;
using NordKredit.Infrastructure;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for Card entity EF Core configuration in NordKreditDbContext.
/// Verifies SQL schema mapping matches COBOL source: CVACT02Y.cpy (CARD-RECORD).
/// </summary>
public class CardDbContextConfigurationTests
{
    private static NordKreditDbContext CreateContext()
    {
        var options = new DbContextOptionsBuilder<NordKreditDbContext>()
            .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
            .Options;

        return new NordKreditDbContext(options);
    }

    [Fact]
    public void Card_ShouldBeConfiguredInDbContext()
    {
        using var context = CreateContext();
        var entityType = context.Model.FindEntityType(typeof(Card));

        Assert.NotNull(entityType);
    }

    [Fact]
    public void Card_PrimaryKey_ShouldBeCardNumber()
    {
        // COBOL: CARDDAT primary key = CARD-NUM PIC X(16)
        using var context = CreateContext();
        var entityType = context.Model.FindEntityType(typeof(Card))!;
        var primaryKey = entityType.FindPrimaryKey()!;

        Assert.Single(primaryKey.Properties);
        Assert.Equal(nameof(Card.CardNumber), primaryKey.Properties[0].Name);
    }

    [Fact]
    public void Card_CardNumber_MaxLength16()
    {
        // COBOL: CARD-NUM PIC X(16)
        using var context = CreateContext();
        var property = context.Model.FindEntityType(typeof(Card))!
            .FindProperty(nameof(Card.CardNumber))!;

        Assert.Equal(16, property.GetMaxLength());
    }

    [Fact]
    public void Card_AccountId_MaxLength11()
    {
        // COBOL: CARD-ACCT-ID PIC 9(11)
        using var context = CreateContext();
        var property = context.Model.FindEntityType(typeof(Card))!
            .FindProperty(nameof(Card.AccountId))!;

        Assert.Equal(11, property.GetMaxLength());
    }

    [Fact]
    public void Card_CvvCode_MaxLength3()
    {
        // COBOL: CARD-CVV-CD PIC 9(03) — PCI-DSS review required
        using var context = CreateContext();
        var property = context.Model.FindEntityType(typeof(Card))!
            .FindProperty(nameof(Card.CvvCode))!;

        Assert.Equal(3, property.GetMaxLength());
    }

    [Fact]
    public void Card_EmbossedName_MaxLength50()
    {
        // COBOL: CARD-EMBOSSED-NAME PIC X(50) — nvarchar(50) for Swedish chars
        using var context = CreateContext();
        var property = context.Model.FindEntityType(typeof(Card))!
            .FindProperty(nameof(Card.EmbossedName))!;

        Assert.Equal(50, property.GetMaxLength());
    }

    [Fact]
    public void Card_RowVersion_IsConcurrencyToken()
    {
        // Replaces COBOL field-by-field comparison with SQL rowversion
        using var context = CreateContext();
        var property = context.Model.FindEntityType(typeof(Card))!
            .FindProperty(nameof(Card.RowVersion))!;

        Assert.True(property.IsConcurrencyToken);
    }

    [Fact]
    public void Card_AccountId_HasIndex()
    {
        // CARDAIX alternate index — enables lookup by account ID
        using var context = CreateContext();
        var entityType = context.Model.FindEntityType(typeof(Card))!;
        var indexes = entityType.GetIndexes().ToList();

        Assert.Contains(indexes, i => i.Properties.Any(p => p.Name == nameof(Card.AccountId)));
    }

    [Fact]
    public void Card_Table_NamedCards()
    {
        using var context = CreateContext();
        var entityType = context.Model.FindEntityType(typeof(Card))!;

        Assert.Equal("Cards", entityType.GetTableName());
    }
}
