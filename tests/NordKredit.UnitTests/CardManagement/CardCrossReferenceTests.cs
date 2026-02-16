using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for the CardCrossReference entity (Card Management domain).
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), 50 bytes.
/// </summary>
public class CardCrossReferenceTests
{
    [Fact]
    public void CardCrossReference_ShouldStoreAllFields()
    {
        var xref = new CardCrossReference
        {
            CardNumber = "4000123456789012",
            CustomerId = "123456789",
            AccountId = "12345678901"
        };

        Assert.Equal("4000123456789012", xref.CardNumber);
        Assert.Equal("123456789", xref.CustomerId);
        Assert.Equal("12345678901", xref.AccountId);
    }

    [Fact]
    public void CardCrossReference_StringProperties_ShouldDefaultToEmpty()
    {
        var xref = new CardCrossReference();

        Assert.Equal(string.Empty, xref.CardNumber);
        Assert.Equal(string.Empty, xref.CustomerId);
        Assert.Equal(string.Empty, xref.AccountId);
    }
}
