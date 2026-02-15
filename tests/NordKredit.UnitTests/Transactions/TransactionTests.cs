using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Tests for the Transaction entity.
/// COBOL source: CVTRA05Y.cpy (TRAN-RECORD)
/// </summary>
public class TransactionTests
{
    [Fact]
    public void Transaction_ShouldStoreAllFields()
    {
        var origTs = new DateTime(2026, 1, 15, 10, 30, 0, DateTimeKind.Utc);
        var procTs = new DateTime(2026, 1, 15, 11, 0, 0, DateTimeKind.Utc);

        var transaction = new Transaction
        {
            Id = "0000000000000001",
            TypeCode = "DB",
            CategoryCode = 1001,
            Source = "ONLINE",
            Description = "Grocery purchase at ICA Maxi",
            Amount = 1234.56m,
            MerchantId = 123456789,
            MerchantName = "ICA Maxi Göteborg",
            MerchantCity = "Göteborg",
            MerchantZip = "41101",
            CardNumber = "4532015112830366",
            OriginationTimestamp = origTs,
            ProcessingTimestamp = procTs
        };

        Assert.Equal("0000000000000001", transaction.Id);
        Assert.Equal("DB", transaction.TypeCode);
        Assert.Equal(1001, transaction.CategoryCode);
        Assert.Equal("ONLINE", transaction.Source);
        Assert.Equal("Grocery purchase at ICA Maxi", transaction.Description);
        Assert.Equal(1234.56m, transaction.Amount);
        Assert.Equal(123456789, transaction.MerchantId);
        Assert.Equal("ICA Maxi Göteborg", transaction.MerchantName);
        Assert.Equal("Göteborg", transaction.MerchantCity);
        Assert.Equal("41101", transaction.MerchantZip);
        Assert.Equal("4532015112830366", transaction.CardNumber);
        Assert.Equal(origTs, transaction.OriginationTimestamp);
        Assert.Equal(procTs, transaction.ProcessingTimestamp);
    }

    [Fact]
    public void Transaction_Amount_ShouldBeDecimal()
    {
        var transaction = new Transaction { Amount = 999999999.99m };

        // COBOL: PIC S9(09)V99 — max value 999,999,999.99
        Assert.Equal(999999999.99m, transaction.Amount);
    }

    [Fact]
    public void Transaction_Amount_ShouldSupportNegativeValues()
    {
        // COBOL: PIC S9(09)V99 — signed field supports negative
        var transaction = new Transaction { Amount = -500.00m };

        Assert.Equal(-500.00m, transaction.Amount);
    }

    [Fact]
    public void Transaction_ShouldSupportSwedishCharacters()
    {
        // EBCDIC-to-UTF-8: Swedish characters (Å, Ä, Ö)
        var transaction = new Transaction
        {
            MerchantName = "Systembolaget Ängelholm",
            MerchantCity = "Malmö",
            Description = "Köp av dagligvaror på Åhléns"
        };

        Assert.Contains("Ä", transaction.MerchantName);
        Assert.Contains("ö", transaction.MerchantCity);
        Assert.Contains("Å", transaction.Description);
    }
}
