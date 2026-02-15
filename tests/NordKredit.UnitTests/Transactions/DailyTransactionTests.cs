using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Tests for the DailyTransaction entity.
/// COBOL source: CVTRA06Y.cpy (DALYTRAN-RECORD)
/// </summary>
public class DailyTransactionTests
{
    [Fact]
    public void DailyTransaction_ShouldStoreAllFields()
    {
        var origTs = new DateTime(2026, 2, 10, 8, 0, 0, DateTimeKind.Utc);
        var procTs = new DateTime(2026, 2, 10, 23, 30, 0, DateTimeKind.Utc);

        var dailyTran = new DailyTransaction
        {
            Id = "0000000000000042",
            TypeCode = "CR",
            CategoryCode = 2001,
            Source = "BATCH",
            Description = "Salary deposit Nordea",
            Amount = 35000.00m,
            MerchantId = 987654321,
            MerchantName = "Nordea Bank AB",
            MerchantCity = "Stockholm",
            MerchantZip = "10571",
            CardNumber = "5425233430109903",
            OriginationTimestamp = origTs,
            ProcessingTimestamp = procTs
        };

        Assert.Equal("0000000000000042", dailyTran.Id);
        Assert.Equal("CR", dailyTran.TypeCode);
        Assert.Equal(2001, dailyTran.CategoryCode);
        Assert.Equal("BATCH", dailyTran.Source);
        Assert.Equal("Salary deposit Nordea", dailyTran.Description);
        Assert.Equal(35000.00m, dailyTran.Amount);
        Assert.Equal(987654321, dailyTran.MerchantId);
        Assert.Equal("Nordea Bank AB", dailyTran.MerchantName);
        Assert.Equal("Stockholm", dailyTran.MerchantCity);
        Assert.Equal("10571", dailyTran.MerchantZip);
        Assert.Equal("5425233430109903", dailyTran.CardNumber);
        Assert.Equal(origTs, dailyTran.OriginationTimestamp);
        Assert.Equal(procTs, dailyTran.ProcessingTimestamp);
    }

    [Fact]
    public void DailyTransaction_Amount_ShouldBeDecimal()
    {
        var dailyTran = new DailyTransaction { Amount = 999999999.99m };

        // COBOL: PIC S9(09)V99 — same layout as TRAN-RECORD
        Assert.Equal(999999999.99m, dailyTran.Amount);
    }

    [Fact]
    public void DailyTransaction_ShouldSupportSwedishCharacters()
    {
        var dailyTran = new DailyTransaction
        {
            MerchantName = "Göteborgs Spårvägar",
            MerchantCity = "Västerås",
            Description = "Månadskort Örebro länstrafik"
        };

        Assert.Contains("ö", dailyTran.MerchantName);
        Assert.Contains("ä", dailyTran.MerchantCity);
        Assert.Contains("Ö", dailyTran.Description);
    }
}
