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

// Card Management domain services (COCRDLIC — card list, COCRDSLC — card detail, COCRDUPC — card update)
builder.Services.AddScoped<NordKredit.Domain.CardManagement.CardListService>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.CardDetailService>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.CardUpdateService>();

// Infrastructure bindings — Azure SQL repositories replace VSAM file I/O
builder.Services.AddScoped<ITransactionRepository, SqlTransactionRepository>();
builder.Services.AddScoped<ITransactionIdGenerator, SqlTransactionIdGenerator>();
builder.Services.AddScoped<ICardCrossReferenceRepository, SqlCardCrossReferenceRepository>();

// Card Management domain — Azure SQL repositories for card entity operations
builder.Services.AddScoped<NordKredit.Domain.CardManagement.ICardRepository, CardMgmtSqlCardRepo>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.ICardCrossReferenceRepository, CardMgmtSqlXrefRepo>();

// EF Core DbContext — replaces Db2 for z/OS
builder.Services.AddDbContext<NordKreditDbContext>();

var app = builder.Build();

app.MapGet("/health", () => Results.Ok(new { Status = "Healthy" }));
app.MapControllers();

app.Run();
