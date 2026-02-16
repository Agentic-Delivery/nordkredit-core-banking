using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.CardManagement;
using NordKredit.Domain.Transactions;
using TransactionCardCrossReference = NordKredit.Domain.Transactions.CardCrossReference;

namespace NordKredit.Infrastructure;

/// <summary>
/// Entity Framework Core DbContext for NordKredit Azure SQL Database.
/// Maps COBOL VSAM/Db2 data structures to Azure SQL tables.
/// </summary>
public class NordKreditDbContext : DbContext
{
    public NordKreditDbContext(DbContextOptions<NordKreditDbContext> options)
        : base(options)
    {
    }

    public DbSet<Card> Cards => Set<Card>();
    public DbSet<Transaction> Transactions => Set<Transaction>();
    public DbSet<DailyTransaction> DailyTransactions => Set<DailyTransaction>();
    public DbSet<TransactionCategoryBalance> TransactionCategoryBalances => Set<TransactionCategoryBalance>();
    public DbSet<TransactionCardCrossReference> CardCrossReferences => Set<TransactionCardCrossReference>();
    public DbSet<Account> Accounts => Set<Account>();
    public DbSet<DailyReject> DailyRejects => Set<DailyReject>();

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        ConfigureCard(modelBuilder);
        ConfigureTransaction(modelBuilder);
        ConfigureDailyTransaction(modelBuilder);
        ConfigureTransactionCategoryBalance(modelBuilder);
        ConfigureCardCrossReference(modelBuilder);
        ConfigureAccount(modelBuilder);
        ConfigureDailyReject(modelBuilder);
    }

    /// <summary>
    /// Card entity mapping. COBOL source: CVACT02Y.cpy (CARD-RECORD), 150 bytes.
    /// VSAM CARDDAT (primary key = CARD-NUM), CARDAIX (alternate index = CARD-ACCT-ID).
    /// Regulations: GDPR Art. 5(1)(c)(d), PSD2 Art. 97.
    /// </summary>
    private static void ConfigureCard(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<Card>(entity =>
        {
            entity.ToTable("Cards");
            entity.HasKey(e => e.CardNumber);

            // COBOL: CARD-NUM PIC X(16) — primary key
            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            // COBOL: CARD-ACCT-ID PIC 9(11) — foreign key to account
            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();
            // COBOL: CARD-CVV-CD PIC 9(03) — PCI-DSS review required
            entity.Property(e => e.CvvCode).HasMaxLength(3).IsRequired();
            // COBOL: CARD-EMBOSSED-NAME PIC X(50) — nvarchar for Swedish chars (Å, Ä, Ö)
            entity.Property(e => e.EmbossedName).HasMaxLength(50).IsRequired();
            // COBOL: CARD-EXPIRAION-DATE PIC X(10) — mapped to DateOnly
            entity.Property(e => e.ExpirationDate).IsRequired();
            // COBOL: CARD-ACTIVE-STATUS PIC X(01) — 'Y'/'N'
            entity.Property(e => e.ActiveStatus).IsRequired();
            // Optimistic concurrency — replaces COBOL field-by-field comparison
            entity.Property(e => e.RowVersion).IsRowVersion();

            // CARDAIX alternate index — enables lookup by account ID
            entity.HasIndex(e => e.AccountId);
        });
    }

    private static void ConfigureTransaction(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<Transaction>(entity =>
        {
            entity.ToTable("Transactions");
            entity.HasKey(e => e.Id);

            entity.Property(e => e.Id).HasMaxLength(16).IsRequired();
            entity.Property(e => e.TypeCode).HasMaxLength(2).IsRequired();
            entity.Property(e => e.CategoryCode).IsRequired();
            entity.Property(e => e.Source).HasMaxLength(10).IsRequired();
            entity.Property(e => e.Description).HasMaxLength(100).IsRequired();
            // COBOL: PIC S9(09)V99 — 9 integer digits + 2 decimal = decimal(11,2)
            entity.Property(e => e.Amount).HasColumnType("decimal(11,2)").IsRequired();
            entity.Property(e => e.MerchantId).IsRequired();
            entity.Property(e => e.MerchantName).HasMaxLength(50).IsRequired();
            entity.Property(e => e.MerchantCity).HasMaxLength(50).IsRequired();
            entity.Property(e => e.MerchantZip).HasMaxLength(10).IsRequired();
            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            entity.Property(e => e.OriginationTimestamp).IsRequired();
            entity.Property(e => e.ProcessingTimestamp).IsRequired();

            entity.HasIndex(e => e.CardNumber);
        });
    }

    private static void ConfigureDailyTransaction(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<DailyTransaction>(entity =>
        {
            entity.ToTable("DailyTransactions");
            entity.HasKey(e => e.Id);

            entity.Property(e => e.Id).HasMaxLength(16).IsRequired();
            entity.Property(e => e.TypeCode).HasMaxLength(2).IsRequired();
            entity.Property(e => e.CategoryCode).IsRequired();
            entity.Property(e => e.Source).HasMaxLength(10).IsRequired();
            entity.Property(e => e.Description).HasMaxLength(100).IsRequired();
            entity.Property(e => e.Amount).HasColumnType("decimal(11,2)").IsRequired();
            entity.Property(e => e.MerchantId).IsRequired();
            entity.Property(e => e.MerchantName).HasMaxLength(50).IsRequired();
            entity.Property(e => e.MerchantCity).HasMaxLength(50).IsRequired();
            entity.Property(e => e.MerchantZip).HasMaxLength(10).IsRequired();
            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            entity.Property(e => e.OriginationTimestamp).IsRequired();
            entity.Property(e => e.ProcessingTimestamp).IsRequired();

            entity.HasIndex(e => e.CardNumber);
        });
    }

    private static void ConfigureTransactionCategoryBalance(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<TransactionCategoryBalance>(entity =>
        {
            entity.ToTable("TransactionCategoryBalances");
            // COBOL: Composite key TRAN-CAT-KEY = ACCT-ID + TYPE-CD + CAT-CD
            entity.HasKey(e => new { e.AccountId, e.TypeCode, e.CategoryCode });

            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();
            entity.Property(e => e.TypeCode).HasMaxLength(2).IsRequired();
            entity.Property(e => e.CategoryCode).IsRequired();
            entity.Property(e => e.Balance).HasColumnType("decimal(11,2)").IsRequired();
        });
    }

    private static void ConfigureCardCrossReference(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<TransactionCardCrossReference>(entity =>
        {
            entity.ToTable("CardCrossReferences");
            entity.HasKey(e => e.CardNumber);

            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            entity.Property(e => e.CustomerId).IsRequired();
            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();

            entity.HasIndex(e => e.AccountId);
            entity.HasIndex(e => e.CustomerId);
        });
    }

    private static void ConfigureAccount(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<Account>(entity =>
        {
            entity.ToTable("Accounts");
            entity.HasKey(e => e.Id);

            entity.Property(e => e.Id).HasMaxLength(11).IsRequired();
            entity.Property(e => e.ActiveStatus).HasMaxLength(1).IsRequired();
            // COBOL: PIC S9(10)V99 — 10 integer digits + 2 decimal = decimal(12,2)
            entity.Property(e => e.CurrentBalance).HasColumnType("decimal(12,2)").IsRequired();
            entity.Property(e => e.CreditLimit).HasColumnType("decimal(12,2)").IsRequired();
            entity.Property(e => e.CashCreditLimit).HasColumnType("decimal(12,2)").IsRequired();
            entity.Property(e => e.CurrentCycleCredit).HasColumnType("decimal(12,2)").IsRequired();
            entity.Property(e => e.CurrentCycleDebit).HasColumnType("decimal(12,2)").IsRequired();
            entity.Property(e => e.ExpirationDate).IsRequired(false);
        });
    }

    private static void ConfigureDailyReject(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<DailyReject>(entity =>
        {
            entity.ToTable("DailyRejects");
            entity.HasKey(e => new { e.TransactionId, e.RejectCode });

            entity.Property(e => e.TransactionId).HasMaxLength(16).IsRequired();
            entity.Property(e => e.CardNumber).HasMaxLength(16).IsRequired();
            entity.Property(e => e.AccountId).HasMaxLength(11).IsRequired();
            entity.Property(e => e.RejectCode).IsRequired();
            entity.Property(e => e.RejectReason).HasMaxLength(100).IsRequired();
            entity.Property(e => e.TransactionAmount).HasColumnType("decimal(11,2)").IsRequired();
            entity.Property(e => e.RejectedAt).IsRequired();
        });
    }
}
