using NordKredit.Functions.Batch;

namespace NordKredit.Functions;

/// <summary>
/// Background worker that triggers the daily batch pipeline on a configurable schedule.
/// Replaces JCL nightly batch trigger for CBTRN01C → CBTRN02C → CBTRN03C.
/// Configuration: BatchSchedule:CronHour and BatchSchedule:CronMinute in appsettings
/// (defaults to 01:00 UTC).
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// </summary>
public partial class Worker : BackgroundService
{
    private readonly IServiceScopeFactory _scopeFactory;
    private readonly TimeProvider _timeProvider;
    private readonly ILogger<Worker> _logger;
    private readonly int _scheduledHour;
    private readonly int _scheduledMinute;

    public Worker(
        IServiceScopeFactory scopeFactory,
        TimeProvider timeProvider,
        IConfiguration configuration,
        ILogger<Worker> logger)
    {
        _scopeFactory = scopeFactory;
        _timeProvider = timeProvider;
        _logger = logger;
        _scheduledHour = configuration.GetValue("BatchSchedule:CronHour", 1);
        _scheduledMinute = configuration.GetValue("BatchSchedule:CronMinute", 0);
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        while (!stoppingToken.IsCancellationRequested)
        {
            var now = _timeProvider.GetUtcNow();
            var nextRun = CalculateNextRun(now);
            var delay = nextRun - now;

            LogNextRunScheduled(_logger, nextRun, delay.TotalMinutes);

            await Task.Delay(delay, _timeProvider, stoppingToken);

            await RunBatchPipelineAsync(stoppingToken);
        }
    }

    private async Task RunBatchPipelineAsync(CancellationToken stoppingToken)
    {
        LogBatchTriggerFired(_logger);

        await using var scope = _scopeFactory.CreateAsyncScope();
        var orchestrator = scope.ServiceProvider.GetRequiredService<DailyBatchOrchestrator>();

        var result = await orchestrator.RunAsync(stoppingToken);

        if (result.Success)
        {
            LogBatchCompleted(_logger, result.Duration.TotalSeconds, result.SlaBreached);
        }
        else
        {
            LogBatchFailed(_logger, result.FailedStep!, result.ErrorMessage!);
        }
    }

    private DateTimeOffset CalculateNextRun(DateTimeOffset now)
    {
        var todayRun = new DateTimeOffset(
            now.UtcDateTime.Date.AddHours(_scheduledHour).AddMinutes(_scheduledMinute),
            TimeSpan.Zero);

        return now < todayRun ? todayRun : todayRun.AddDays(1);
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Daily batch pipeline scheduled for {NextRun:yyyy-MM-dd HH:mm:ss} UTC (in {DelayMinutes:F0} minutes)")]
    private static partial void LogNextRunScheduled(ILogger logger, DateTimeOffset nextRun, double delayMinutes);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Daily batch pipeline trigger fired")]
    private static partial void LogBatchTriggerFired(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Daily batch pipeline completed. Duration: {DurationSeconds:F1}s, SLA breached: {SlaBreached}")]
    private static partial void LogBatchCompleted(ILogger logger, double durationSeconds, bool slaBreached);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Daily batch pipeline FAILED at step {FailedStep}: {ErrorMessage}")]
    private static partial void LogBatchFailed(ILogger logger, string failedStep, string errorMessage);
}
