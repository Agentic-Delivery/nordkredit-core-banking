using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for the Card entity.
/// COBOL source: CVACT02Y.cpy (CARD-RECORD), 150 bytes.
/// </summary>
public class CardTests
{
    [Fact]
    public void Card_ShouldStoreAllFields()
    {
        var expiry = new DateOnly(2027, 12, 31);

        var card = new Card
        {
            CardNumber = "4000123456789012",
            AccountId = "12345678901",
            CvvCode = "123",
            EmbossedName = "JOHN DOE",
            ExpirationDate = expiry,
            ActiveStatus = 'Y'
        };

        Assert.Equal("4000123456789012", card.CardNumber);
        Assert.Equal("12345678901", card.AccountId);
        Assert.Equal("123", card.CvvCode);
        Assert.Equal("JOHN DOE", card.EmbossedName);
        Assert.Equal(expiry, card.ExpirationDate);
        Assert.Equal('Y', card.ActiveStatus);
    }

    [Fact]
    public void Card_ActiveStatus_ShouldSupportInactive()
    {
        // COBOL: CARD-ACTIVE-STATUS PIC X(01) — 'Y' or 'N'
        var card = new Card { ActiveStatus = 'N' };

        Assert.Equal('N', card.ActiveStatus);
    }

    [Fact]
    public void Card_ExpirationDate_ShouldUseDateOnly()
    {
        // COBOL: CARD-EXPIRAION-DATE PIC X(10) format YYYY-MM-DD
        // Migrated to DateOnly for proper date semantics
        var card = new Card { ExpirationDate = new DateOnly(2028, 6, 15) };

        Assert.Equal(2028, card.ExpirationDate.Year);
        Assert.Equal(6, card.ExpirationDate.Month);
        Assert.Equal(15, card.ExpirationDate.Day);
    }

    [Fact]
    public void Card_ShouldSupportSwedishCharacters()
    {
        // EBCDIC-to-UTF-8: Swedish characters (Å, Ä, Ö) in embossed name
        // nvarchar(50) supports Unicode
        var card = new Card
        {
            EmbossedName = "BJÖRK ÅNGSTRÖM"
        };

        Assert.Contains("Ö", card.EmbossedName);
        Assert.Contains("Å", card.EmbossedName);
    }

    [Fact]
    public void Card_RowVersion_ShouldDefaultToNull()
    {
        // rowversion for optimistic concurrency — replaces COBOL field-by-field comparison
        var card = new Card();

        Assert.Null(card.RowVersion);
    }

    [Fact]
    public void Card_RowVersion_ShouldStoreByteArray()
    {
        var card = new Card { RowVersion = [0x00, 0x01, 0x02, 0x03] };

        Assert.NotNull(card.RowVersion);
        Assert.Equal(4, card.RowVersion.Length);
    }

    [Fact]
    public void Card_StringProperties_ShouldDefaultToEmpty()
    {
        var card = new Card();

        Assert.Equal(string.Empty, card.CardNumber);
        Assert.Equal(string.Empty, card.AccountId);
        Assert.Equal(string.Empty, card.CvvCode);
        Assert.Equal(string.Empty, card.EmbossedName);
    }
}
