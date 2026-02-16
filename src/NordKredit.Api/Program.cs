using NordKredit.Domain.CardManagement;
using NordKredit.Domain.Transactions;
using NordKredit.Infrastructure;
using NordKredit.Infrastructure.Transactions;
using CardMgmtSqlCardRepo = NordKredit.Infrastructure.CardManagement.SqlCardRepository;
using CardMgmtSqlXrefRepo = NordKredit.Infrastructure.CardManagement.SqlCardCrossReferenceRepository;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddControllers();

// Transaction domain services (COTRN00C/01C/02C — online transaction processing)
builder.Services.AddScoped<TransactionValidationService>();
builder.Services.AddScoped<TransactionAddService>();
builder.Services.AddScoped<TransactionDetailService>();
builder.Services.AddScoped<TransactionListService>();

// Infrastructure bindings — Azure SQL repositories replace VSAM file I/O
builder.Services.AddScoped<ITransactionRepository, SqlTransactionRepository>();
builder.Services.AddScoped<ITransactionIdGenerator, SqlTransactionIdGenerator>();
builder.Services.AddScoped<NordKredit.Domain.Transactions.ICardCrossReferenceRepository, SqlCardCrossReferenceRepository>();

// Card Management domain — services and Azure SQL repositories
builder.Services.AddScoped<CardListService>();
builder.Services.AddScoped<ICardRepository, CardMgmtSqlCardRepo>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.ICardCrossReferenceRepository, CardMgmtSqlXrefRepo>();

// EF Core DbContext — replaces Db2 for z/OS
builder.Services.AddDbContext<NordKreditDbContext>();

var app = builder.Build();

app.MapGet("/health", () => Results.Ok(new { Status = "Healthy" }));
app.MapControllers();

app.Run();
