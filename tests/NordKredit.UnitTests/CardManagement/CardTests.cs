using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for the Card entity.
/// COBOL source: CVACT02Y.cpy (CARD-RECORD), 150 bytes.
/// Regulations: GDPR Art. 5(1)(c)(d), PSD2 Art. 97.
/// </summary>
public class CardTests
{
    [Fact]
    public void Card_ShouldStoreAllFields()
    {
        var card = new Card
        {
            CardNumber = "4000123456789012",
            AccountId = "12345678901",
            CvvCode = "123",
            EmbossedName = "JOHN DOE",
            ExpirationDate = new DateOnly(2027, 12, 31),
            ActiveStatus = 'Y',
            RowVersion = [0x00, 0x00, 0x00, 0x01]
        };

        Assert.Equal("4000123456789012", card.CardNumber);
        Assert.Equal("12345678901", card.AccountId);
        Assert.Equal("123", card.CvvCode);
        Assert.Equal("JOHN DOE", card.EmbossedName);
        Assert.Equal(new DateOnly(2027, 12, 31), card.ExpirationDate);
        Assert.Equal('Y', card.ActiveStatus);
        Assert.Equal([0x00, 0x00, 0x00, 0x01], card.RowVersion);
    }

    [Fact]
    public void Card_CardNumber_ShouldBe16Characters()
    {
        // COBOL: CARD-NUM PIC X(16) — 16-digit card number
        var card = new Card { CardNumber = "4000123456789012" };

        Assert.Equal(16, card.CardNumber.Length);
    }

    [Fact]
    public void Card_AccountId_ShouldBe11Characters()
    {
        // COBOL: CARD-ACCT-ID PIC 9(11) — 11-digit account identifier
        var card = new Card { AccountId = "12345678901" };

        Assert.Equal(11, card.AccountId.Length);
    }

    [Fact]
    public void Card_CvvCode_ShouldBe3Characters()
    {
        // COBOL: CARD-CVV-CD PIC 9(03) — 3-digit CVV
        var card = new Card { CvvCode = "987" };

        Assert.Equal(3, card.CvvCode.Length);
    }

    [Fact]
    public void Card_ExpirationDate_ShouldUseDateOnly()
    {
        // COBOL: CARD-EXPIRAION-DATE PIC X(10) format YYYY-MM-DD
        // Mapped to DateOnly (no time component needed)
        var card = new Card { ExpirationDate = new DateOnly(2028, 6, 15) };

        Assert.Equal(2028, card.ExpirationDate.Year);
        Assert.Equal(6, card.ExpirationDate.Month);
        Assert.Equal(15, card.ExpirationDate.Day);
    }

    [Theory]
    [InlineData('Y')]
    [InlineData('N')]
    public void Card_ActiveStatus_ShouldAcceptValidValues(char status)
    {
        // COBOL: CARD-ACTIVE-STATUS PIC X(01) — 'Y' or 'N'
        var card = new Card { ActiveStatus = status };

        Assert.Equal(status, card.ActiveStatus);
    }

    [Fact]
    public void Card_EmbossedName_ShouldSupportSwedishCharacters()
    {
        // EBCDIC→UTF-8: Swedish characters (Å, Ä, Ö) must be preserved
        // Azure SQL nvarchar(50) supports full Unicode
        var card = new Card { EmbossedName = "BJÖRK ÅSTRÖM" };

        Assert.Contains("Ö", card.EmbossedName);
        Assert.Contains("Å", card.EmbossedName);
    }

    [Fact]
    public void Card_EmbossedName_MaxLength50()
    {
        // COBOL: CARD-EMBOSSED-NAME PIC X(50) — max 50 characters
        var name = new string('A', 50);
        var card = new Card { EmbossedName = name };

        Assert.Equal(50, card.EmbossedName.Length);
    }

    [Fact]
    public void Card_RowVersion_DefaultsToEmptyArray()
    {
        var card = new Card();

        Assert.Empty(card.RowVersion);
    }

    [Fact]
    public void Card_StringProperties_DefaultToEmpty()
    {
        var card = new Card();

        Assert.Equal(string.Empty, card.CardNumber);
        Assert.Equal(string.Empty, card.AccountId);
        Assert.Equal(string.Empty, card.CvvCode);
        Assert.Equal(string.Empty, card.EmbossedName);
    }
}
