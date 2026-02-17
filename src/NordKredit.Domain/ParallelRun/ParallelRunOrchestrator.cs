using System.Diagnostics;
using System.Text.Json;
using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Orchestrates parallel-run execution: sends requests to both mainframe and Azure,
/// captures outputs, delegates comparison, and persists results for audit trail.
/// The mainframe is the system of record during parallel-run.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public partial class ParallelRunOrchestrator
{
    private readonly IMainframeGateway _mainframeGateway;
    private readonly IComparisonEngine _comparisonEngine;
    private readonly IDivergenceStore _divergenceStore;
    private readonly ParallelRunConfiguration _config;
    private readonly ILogger<ParallelRunOrchestrator> _logger;

    public ParallelRunOrchestrator(
        IMainframeGateway mainframeGateway,
        IComparisonEngine comparisonEngine,
        IDivergenceStore divergenceStore,
        ParallelRunConfiguration config,
        ILogger<ParallelRunOrchestrator> logger)
    {
        _mainframeGateway = mainframeGateway;
        _comparisonEngine = comparisonEngine;
        _divergenceStore = divergenceStore;
        _config = config;
        _logger = logger;
    }

    /// <summary>
    /// Executes a parallel-run comparison for a single request.
    /// If the domain is not enabled, returns the Azure response without calling the mainframe.
    /// </summary>
    /// <param name="domain">The domain (e.g., "CardManagement", "Transactions").</param>
    /// <param name="operation">The operation (e.g., "CardDetail", "TransactionList").</param>
    /// <param name="requestPayload">The request payload as JSON.</param>
    /// <param name="azureResponse">The response from the Azure system (already computed).</param>
    /// <param name="correlationId">Correlation ID for tracing.</param>
    /// <param name="accountId">Optional account ID for divergence tracking.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    public async Task<ParallelRunResult> ExecuteAsync(
        string domain,
        string operation,
        string requestPayload,
        string azureResponse,
        string correlationId,
        string? accountId = null,
        CancellationToken cancellationToken = default)
    {
        var stopwatch = Stopwatch.StartNew();

        if (!_config.IsDomainEnabled(domain))
        {
            LogDomainDisabled(_logger, domain);
            stopwatch.Stop();
            return new ParallelRunResult
            {
                MainframeResponse = null,
                AzureResponse = azureResponse,
                Comparison = null,
                BothSystemsResponded = false,
                MainframeTimedOut = false,
                CorrelationId = correlationId,
                Duration = stopwatch.Elapsed
            };
        }

        // Call mainframe with timeout
        string? mainframeResponse = null;
        bool mainframeTimedOut = false;
        string? errorMessage = null;

        try
        {
            using var timeoutCts = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            timeoutCts.CancelAfter(_config.MainframeTimeout);

            LogMainframeCallStarted(_logger, domain, operation, correlationId);
            mainframeResponse = await _mainframeGateway.SendAsync(
                domain, operation, requestPayload, correlationId, timeoutCts.Token);
            LogMainframeCallCompleted(_logger, domain, operation, correlationId);
        }
        catch (OperationCanceledException) when (!cancellationToken.IsCancellationRequested)
        {
            mainframeTimedOut = true;
            LogMainframeTimeout(_logger, domain, operation, correlationId, _config.MainframeTimeout.TotalSeconds);
        }
        catch (Exception ex)
        {
            errorMessage = ex.Message;
            LogMainframeError(_logger, domain, operation, correlationId, ex.Message);
        }

        // Compare outputs if both responded
        ComparisonResult? comparison = null;
        if (mainframeResponse is not null)
        {
            comparison = _comparisonEngine.Compare(
                domain, operation, mainframeResponse, azureResponse, correlationId);

            LogComparisonResult(_logger, domain, operation, correlationId, comparison.IsMatch, comparison.Divergences.Count);

            // Persist for audit trail
            var record = new DivergenceRecord
            {
                Id = Guid.NewGuid().ToString(),
                Domain = domain,
                Operation = operation,
                CorrelationId = correlationId,
                IsMatch = comparison.IsMatch,
                MainframeResponse = mainframeResponse,
                AzureResponse = azureResponse,
                DivergencesJson = comparison.Divergences.Count > 0
                    ? JsonSerializer.Serialize(comparison.Divergences)
                    : null,
                AccountId = accountId,
                RecordedAt = DateTimeOffset.UtcNow
            };

            await _divergenceStore.SaveAsync(record, cancellationToken);
        }

        stopwatch.Stop();

        return new ParallelRunResult
        {
            MainframeResponse = mainframeResponse,
            AzureResponse = azureResponse,
            Comparison = comparison,
            BothSystemsResponded = mainframeResponse is not null,
            MainframeTimedOut = mainframeTimedOut,
            ErrorMessage = errorMessage,
            CorrelationId = correlationId,
            Duration = stopwatch.Elapsed
        };
    }

    [LoggerMessage(Level = LogLevel.Debug,
        Message = "Parallel-run skipped for domain {Domain} — not enabled")]
    private static partial void LogDomainDisabled(ILogger logger, string domain);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Parallel-run: calling mainframe for {Domain}/{Operation} (correlation: {CorrelationId})")]
    private static partial void LogMainframeCallStarted(ILogger logger, string domain, string operation, string correlationId);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Parallel-run: mainframe responded for {Domain}/{Operation} (correlation: {CorrelationId})")]
    private static partial void LogMainframeCallCompleted(ILogger logger, string domain, string operation, string correlationId);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Parallel-run: mainframe timed out for {Domain}/{Operation} (correlation: {CorrelationId}, timeout: {TimeoutSeconds}s)")]
    private static partial void LogMainframeTimeout(ILogger logger, string domain, string operation, string correlationId, double timeoutSeconds);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Parallel-run: mainframe error for {Domain}/{Operation} (correlation: {CorrelationId}): {ErrorMessage}")]
    private static partial void LogMainframeError(ILogger logger, string domain, string operation, string correlationId, string errorMessage);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Parallel-run comparison: {Domain}/{Operation} (correlation: {CorrelationId}) — match: {IsMatch}, divergences: {DivergenceCount}")]
    private static partial void LogComparisonResult(ILogger logger, string domain, string operation, string correlationId, bool isMatch, int divergenceCount);
}
