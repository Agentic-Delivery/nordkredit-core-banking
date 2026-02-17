using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.CardManagement;
using NordKredit.Domain.Transactions;
using NordKredit.Infrastructure;
using TransactionCardCrossReference = NordKredit.Domain.Transactions.CardCrossReference;

namespace NordKredit.UnitTests.Infrastructure;

/// <summary>
/// Tests for the initial EF Core migration schema.
/// Verifies that all 9 tables from NordKreditDbContext are correctly mapped
/// with column types matching COBOL PIC field sizes, indexes, and composite keys.
/// Uses SQL Server provider for model inspection (no actual database connection).
/// </summary>
public class InitialMigrationTests
{
    private static NordKreditDbContext CreateContext()
    {
        var options = new DbContextOptionsBuilder<NordKreditDbContext>()
            .UseSqlServer("Server=.;Database=Test;Trusted_Connection=True;")
            .Options;

        return new NordKreditDbContext(options);
    }

    [Fact]
    public void Migration_Creates_All_Nine_Tables()
    {
        using var context = CreateContext();
        var entityTypes = context.Model.GetEntityTypes().ToList();

        string[] expectedTables =
        [
            "Cards", "Transactions", "DailyTransactions", "TransactionCategoryBalances",
            "CardCrossReferences", "Accounts", "DailyRejects", "TransactionTypes",
            "TransactionCategories"
        ];

        foreach (string table in expectedTables)
        {
            Assert.Contains(entityTypes, e => e.GetTableName() == table);
        }
    }

    [Fact]
    public void Accounts_Table_HasCorrectSchema()
    {
        // COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD, 300 bytes)
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(Account))!;

        Assert.Equal("Accounts", entity.GetTableName());
        Assert.Equal("Id", entity.FindPrimaryKey()!.Properties[0].Name);
        Assert.Equal(11, entity.FindProperty("Id")!.GetMaxLength());

        // COBOL: ACCT-CURR-BAL PIC S9(10)V99 = decimal(12,2)
        Assert.Equal("decimal(12,2)", entity.FindProperty("CurrentBalance")!.GetColumnType());
        Assert.Equal("decimal(12,2)", entity.FindProperty("CreditLimit")!.GetColumnType());
        Assert.Equal("decimal(12,2)", entity.FindProperty("CashCreditLimit")!.GetColumnType());
        Assert.Equal("decimal(12,2)", entity.FindProperty("CurrentCycleCredit")!.GetColumnType());
        Assert.Equal("decimal(12,2)", entity.FindProperty("CurrentCycleDebit")!.GetColumnType());

        // ExpirationDate is nullable
        Assert.True(entity.FindProperty("ExpirationDate")!.IsNullable);
    }

    [Fact]
    public void Transactions_Table_HasCorrectDecimalPrecision()
    {
        // COBOL: PIC S9(09)V99 = decimal(11,2) — must NOT be floating-point
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(Transaction))!;

        Assert.Equal("Transactions", entity.GetTableName());
        Assert.Equal("decimal(11,2)", entity.FindProperty("Amount")!.GetColumnType());
    }

    [Fact]
    public void Transactions_Table_HasCardNumberIndex()
    {
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(Transaction))!;
        var indexes = entity.GetIndexes().ToList();

        Assert.Contains(indexes, i => i.Properties.Any(p => p.Name == "CardNumber"));
    }

    [Fact]
    public void DailyTransactions_Table_HasCorrectDecimalPrecision()
    {
        // COBOL: DALYTRAN-AMT PIC S9(09)V99 = decimal(11,2)
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(DailyTransaction))!;

        Assert.Equal("DailyTransactions", entity.GetTableName());
        Assert.Equal("decimal(11,2)", entity.FindProperty("Amount")!.GetColumnType());
    }

    [Fact]
    public void DailyTransactions_Table_HasCardNumberIndex()
    {
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(DailyTransaction))!;
        var indexes = entity.GetIndexes().ToList();

        Assert.Contains(indexes, i => i.Properties.Any(p => p.Name == "CardNumber"));
    }

    [Fact]
    public void TransactionCategoryBalances_HasCompositeKey()
    {
        // COBOL: TRAN-CAT-KEY = ACCT-ID + TYPE-CD + CAT-CD
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(TransactionCategoryBalance))!;
        var pk = entity.FindPrimaryKey()!;

        Assert.Equal(3, pk.Properties.Count);
        Assert.Equal("AccountId", pk.Properties[0].Name);
        Assert.Equal("TypeCode", pk.Properties[1].Name);
        Assert.Equal("CategoryCode", pk.Properties[2].Name);
    }

    [Fact]
    public void TransactionCategoryBalances_HasCorrectDecimalPrecision()
    {
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(TransactionCategoryBalance))!;

        Assert.Equal("decimal(11,2)", entity.FindProperty("Balance")!.GetColumnType());
    }

    [Fact]
    public void CardCrossReferences_HasCorrectIndexes()
    {
        // CVACT03Y.cpy: indexes on AccountId and CustomerId
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(TransactionCardCrossReference))!;
        var indexes = entity.GetIndexes().ToList();

        Assert.Contains(indexes, i => i.Properties.Any(p => p.Name == "AccountId"));
        Assert.Contains(indexes, i => i.Properties.Any(p => p.Name == "CustomerId"));
    }

    [Fact]
    public void DailyRejects_HasCompositeKey()
    {
        // Composite key: TransactionId + RejectCode
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(DailyReject))!;
        var pk = entity.FindPrimaryKey()!;

        Assert.Equal(2, pk.Properties.Count);
        Assert.Equal("TransactionId", pk.Properties[0].Name);
        Assert.Equal("RejectCode", pk.Properties[1].Name);
    }

    [Fact]
    public void DailyRejects_HasCorrectDecimalPrecision()
    {
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(DailyReject))!;

        Assert.Equal("decimal(11,2)", entity.FindProperty("TransactionAmount")!.GetColumnType());
    }

    [Fact]
    public void TransactionTypes_HasCorrectPrimaryKey()
    {
        // COBOL: TRAN-TYPE PIC X(02) = primary key
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(TransactionType))!;

        Assert.Equal("TransactionTypes", entity.GetTableName());
        Assert.Equal("TypeCode", entity.FindPrimaryKey()!.Properties[0].Name);
        Assert.Equal(2, entity.FindProperty("TypeCode")!.GetMaxLength());
    }

    [Fact]
    public void TransactionCategories_HasCompositeKey()
    {
        // COBOL: composite key = TRAN-TYPE PIC X(02) + TRAN-CAT-CD PIC 9(04)
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(TransactionCategory))!;
        var pk = entity.FindPrimaryKey()!;

        Assert.Equal(2, pk.Properties.Count);
        Assert.Equal("TypeCode", pk.Properties[0].Name);
        Assert.Equal("CategoryCode", pk.Properties[1].Name);
    }

    [Fact]
    public void Cards_HasRowVersionConcurrencyToken()
    {
        using var context = CreateContext();
        var entity = context.Model.FindEntityType(typeof(Card))!;
        var prop = entity.FindProperty("RowVersion")!;

        Assert.True(prop.IsConcurrencyToken);
    }

    [Fact]
    public void AllFinancialAmounts_UseDecimalNotFloat()
    {
        // Regulatory requirement: no floating-point for financial amounts
        using var context = CreateContext();

        var decimalProperties = context.Model.GetEntityTypes()
            .SelectMany(e => e.GetProperties())
            .Where(p => p.ClrType == typeof(decimal));

        foreach (var prop in decimalProperties)
        {
            var columnType = prop.GetColumnType();
            Assert.NotNull(columnType);
            Assert.StartsWith("decimal(", columnType);
        }
    }
}
