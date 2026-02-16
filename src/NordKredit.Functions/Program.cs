using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;
using NordKredit.Functions;
using NordKredit.Functions.Batch;
using NordKredit.Infrastructure;
using NordKredit.Infrastructure.Transactions;

var builder = Host.CreateApplicationBuilder(args);

builder.Services.AddDbContext<NordKreditDbContext>(options =>
    options.UseSqlServer(builder.Configuration.GetConnectionString("NordKreditDb")));

builder.Services.AddScoped<IDailyTransactionRepository, SqlDailyTransactionRepository>();
builder.Services.AddScoped<ICardCrossReferenceRepository, SqlCardCrossReferenceRepository>();
builder.Services.AddScoped<IAccountRepository, SqlAccountRepository>();
builder.Services.AddScoped<CardVerificationService>();
builder.Services.AddScoped<CardVerificationFunction>();

builder.Services.AddHostedService<Worker>();

var host = builder.Build();
host.Run();
