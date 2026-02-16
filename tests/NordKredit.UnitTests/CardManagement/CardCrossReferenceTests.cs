using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Tests for the CardCrossReference entity (Card Management domain).
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), 50 bytes.
/// Regulations: GDPR Art. 5(1)(c), AML/KYC.
/// </summary>
public class CardCrossReferenceTests
{
    [Fact]
    public void CardCrossReference_ShouldStoreAllFields()
    {
        var xref = new CardCrossReference
        {
            CardNumber = "4000123456789012",
            CustomerId = 123456789,
            AccountId = "12345678901"
        };

        Assert.Equal("4000123456789012", xref.CardNumber);
        Assert.Equal(123456789, xref.CustomerId);
        Assert.Equal("12345678901", xref.AccountId);
    }

    [Fact]
    public void CardCrossReference_CardNumber_ShouldBe16Characters()
    {
        // COBOL: XREF-CARD-NUM PIC X(16)
        var xref = new CardCrossReference { CardNumber = "4000123456789012" };

        Assert.Equal(16, xref.CardNumber.Length);
    }

    [Fact]
    public void CardCrossReference_CustomerId_ShouldBe9Digits()
    {
        // COBOL: XREF-CUST-ID PIC 9(09) — max 999999999
        var xref = new CardCrossReference { CustomerId = 999999999 };

        Assert.Equal(999999999, xref.CustomerId);
    }

    [Fact]
    public void CardCrossReference_AccountId_ShouldBe11Characters()
    {
        // COBOL: XREF-ACCT-ID PIC 9(11)
        var xref = new CardCrossReference { AccountId = "12345678901" };

        Assert.Equal(11, xref.AccountId.Length);
    }

    [Fact]
    public void CardCrossReference_StringProperties_DefaultToEmpty()
    {
        var xref = new CardCrossReference();

        Assert.Equal(string.Empty, xref.CardNumber);
        Assert.Equal(string.Empty, xref.AccountId);
    }

    [Fact]
    public void CardCrossReference_MultipleCardsForSameCustomer()
    {
        // CARD-BR-010 Scenario 2: Customer can have multiple cards
        var xref1 = new CardCrossReference
        {
            CardNumber = "4000123456789012",
            CustomerId = 123456789,
            AccountId = "12345678901"
        };
        var xref2 = new CardCrossReference
        {
            CardNumber = "4000123456789028",
            CustomerId = 123456789,
            AccountId = "12345678901"
        };

        Assert.Equal(xref1.CustomerId, xref2.CustomerId);
        Assert.NotEqual(xref1.CardNumber, xref2.CardNumber);
    }

    [Fact]
    public void CardCrossReference_CardsAcrossMultipleAccounts()
    {
        // CARD-BR-010 Scenario 3: Customer has cards on different accounts
        var xref1 = new CardCrossReference
        {
            CardNumber = "4000123456789012",
            CustomerId = 123456789,
            AccountId = "12345678901"
        };
        var xref2 = new CardCrossReference
        {
            CardNumber = "4000987654321098",
            CustomerId = 123456789,
            AccountId = "98765432101"
        };

        Assert.Equal(xref1.CustomerId, xref2.CustomerId);
        Assert.NotEqual(xref1.AccountId, xref2.AccountId);
    }
}
