using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Tests for the TransactionCategoryBalance entity.
/// COBOL source: CVTRA01Y.cpy (TRAN-CAT-BAL-RECORD)
/// </summary>
public class TransactionCategoryBalanceTests
{
    [Fact]
    public void TransactionCategoryBalance_ShouldStoreAllFields()
    {
        var balance = new TransactionCategoryBalance
        {
            AccountId = "00000000001",
            TypeCode = "DB",
            CategoryCode = 5001,
            Balance = 45678.90m
        };

        Assert.Equal("00000000001", balance.AccountId);
        Assert.Equal("DB", balance.TypeCode);
        Assert.Equal(5001, balance.CategoryCode);
        Assert.Equal(45678.90m, balance.Balance);
    }

    [Fact]
    public void TransactionCategoryBalance_Balance_ShouldBeDecimal()
    {
        var balance = new TransactionCategoryBalance { Balance = 999999999.99m };

        // COBOL: PIC S9(09)V99 — same as transaction amount
        Assert.Equal(999999999.99m, balance.Balance);
    }

    [Fact]
    public void TransactionCategoryBalance_Balance_ShouldSupportNegativeValues()
    {
        // COBOL: PIC S9(09)V99 — signed field
        var balance = new TransactionCategoryBalance { Balance = -1234.56m };

        Assert.Equal(-1234.56m, balance.Balance);
    }
}
