using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace NordKredit.Infrastructure.Migrations;

/// <inheritdoc />
public partial class InitialCreate : Migration
{
    /// <inheritdoc />
    protected override void Up(MigrationBuilder migrationBuilder)
    {
        migrationBuilder.CreateTable(
            name: "Accounts",
            columns: table => new
            {
                Id = table.Column<string>(type: "nvarchar(11)", maxLength: 11, nullable: false),
                ActiveStatus = table.Column<string>(type: "nvarchar(1)", maxLength: 1, nullable: false),
                CurrentBalance = table.Column<decimal>(type: "decimal(12,2)", nullable: false),
                CreditLimit = table.Column<decimal>(type: "decimal(12,2)", nullable: false),
                CashCreditLimit = table.Column<decimal>(type: "decimal(12,2)", nullable: false),
                CurrentCycleCredit = table.Column<decimal>(type: "decimal(12,2)", nullable: false),
                CurrentCycleDebit = table.Column<decimal>(type: "decimal(12,2)", nullable: false),
                ExpirationDate = table.Column<DateTime>(type: "datetime2", nullable: true)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_Accounts", x => x.Id);
            });

        migrationBuilder.CreateTable(
            name: "CardCrossReferences",
            columns: table => new
            {
                CardNumber = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                CustomerId = table.Column<int>(type: "int", nullable: false),
                AccountId = table.Column<string>(type: "nvarchar(11)", maxLength: 11, nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_CardCrossReferences", x => x.CardNumber);
            });

        migrationBuilder.CreateTable(
            name: "Cards",
            columns: table => new
            {
                CardNumber = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                AccountId = table.Column<string>(type: "nvarchar(11)", maxLength: 11, nullable: false),
                CvvCode = table.Column<string>(type: "nvarchar(3)", maxLength: 3, nullable: false),
                EmbossedName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                ExpirationDate = table.Column<DateOnly>(type: "date", nullable: false),
                ActiveStatus = table.Column<string>(type: "nvarchar(1)", nullable: false),
                RowVersion = table.Column<byte[]>(type: "rowversion", rowVersion: true, nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_Cards", x => x.CardNumber);
            });

        migrationBuilder.CreateTable(
            name: "DailyRejects",
            columns: table => new
            {
                TransactionId = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                RejectCode = table.Column<int>(type: "int", nullable: false),
                CardNumber = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                AccountId = table.Column<string>(type: "nvarchar(11)", maxLength: 11, nullable: false),
                RejectReason = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                TransactionAmount = table.Column<decimal>(type: "decimal(11,2)", nullable: false),
                RejectedAt = table.Column<DateTime>(type: "datetime2", nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_DailyRejects", x => new { x.TransactionId, x.RejectCode });
            });

        migrationBuilder.CreateTable(
            name: "DailyTransactions",
            columns: table => new
            {
                Id = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                TypeCode = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                CategoryCode = table.Column<int>(type: "int", nullable: false),
                Source = table.Column<string>(type: "nvarchar(10)", maxLength: 10, nullable: false),
                Description = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                Amount = table.Column<decimal>(type: "decimal(11,2)", nullable: false),
                MerchantId = table.Column<int>(type: "int", nullable: false),
                MerchantName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                MerchantCity = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                MerchantZip = table.Column<string>(type: "nvarchar(10)", maxLength: 10, nullable: false),
                CardNumber = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                OriginationTimestamp = table.Column<DateTime>(type: "datetime2", nullable: false),
                ProcessingTimestamp = table.Column<DateTime>(type: "datetime2", nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_DailyTransactions", x => x.Id);
            });

        migrationBuilder.CreateTable(
            name: "TransactionCategories",
            columns: table => new
            {
                TypeCode = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                CategoryCode = table.Column<int>(type: "int", nullable: false),
                Description = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_TransactionCategories", x => new { x.TypeCode, x.CategoryCode });
            });

        migrationBuilder.CreateTable(
            name: "TransactionCategoryBalances",
            columns: table => new
            {
                AccountId = table.Column<string>(type: "nvarchar(11)", maxLength: 11, nullable: false),
                TypeCode = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                CategoryCode = table.Column<int>(type: "int", nullable: false),
                Balance = table.Column<decimal>(type: "decimal(11,2)", nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_TransactionCategoryBalances", x => new { x.AccountId, x.TypeCode, x.CategoryCode });
            });

        migrationBuilder.CreateTable(
            name: "Transactions",
            columns: table => new
            {
                Id = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                TypeCode = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                CategoryCode = table.Column<int>(type: "int", nullable: false),
                Source = table.Column<string>(type: "nvarchar(10)", maxLength: 10, nullable: false),
                Description = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                Amount = table.Column<decimal>(type: "decimal(11,2)", nullable: false),
                MerchantId = table.Column<int>(type: "int", nullable: false),
                MerchantName = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                MerchantCity = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                MerchantZip = table.Column<string>(type: "nvarchar(10)", maxLength: 10, nullable: false),
                CardNumber = table.Column<string>(type: "nvarchar(16)", maxLength: 16, nullable: false),
                OriginationTimestamp = table.Column<DateTime>(type: "datetime2", nullable: false),
                ProcessingTimestamp = table.Column<DateTime>(type: "datetime2", nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_Transactions", x => x.Id);
            });

        migrationBuilder.CreateTable(
            name: "TransactionTypes",
            columns: table => new
            {
                TypeCode = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                Description = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false)
            },
            constraints: table =>
            {
                table.PrimaryKey("PK_TransactionTypes", x => x.TypeCode);
            });

        migrationBuilder.CreateIndex(
            name: "IX_CardCrossReferences_AccountId",
            table: "CardCrossReferences",
            column: "AccountId");

        migrationBuilder.CreateIndex(
            name: "IX_CardCrossReferences_CustomerId",
            table: "CardCrossReferences",
            column: "CustomerId");

        migrationBuilder.CreateIndex(
            name: "IX_Cards_AccountId",
            table: "Cards",
            column: "AccountId");

        migrationBuilder.CreateIndex(
            name: "IX_DailyTransactions_CardNumber",
            table: "DailyTransactions",
            column: "CardNumber");

        migrationBuilder.CreateIndex(
            name: "IX_Transactions_CardNumber",
            table: "Transactions",
            column: "CardNumber");
    }

    /// <inheritdoc />
    protected override void Down(MigrationBuilder migrationBuilder)
    {
        migrationBuilder.DropTable(
            name: "Accounts");

        migrationBuilder.DropTable(
            name: "CardCrossReferences");

        migrationBuilder.DropTable(
            name: "Cards");

        migrationBuilder.DropTable(
            name: "DailyRejects");

        migrationBuilder.DropTable(
            name: "DailyTransactions");

        migrationBuilder.DropTable(
            name: "TransactionCategories");

        migrationBuilder.DropTable(
            name: "TransactionCategoryBalances");

        migrationBuilder.DropTable(
            name: "Transactions");

        migrationBuilder.DropTable(
            name: "TransactionTypes");
    }
}
