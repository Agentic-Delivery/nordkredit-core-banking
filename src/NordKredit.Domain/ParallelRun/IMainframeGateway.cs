namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Gateway for calling the mainframe system during parallel-run.
/// Infrastructure layer implements via HTTP or Azure Service Bus.
/// The mainframe is the system of record during parallel-run.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public interface IMainframeGateway
{
    /// <summary>
    /// Sends a request to the mainframe and returns the response as a JSON string.
    /// </summary>
    /// <param name="domain">The domain (e.g., "CardManagement", "Transactions").</param>
    /// <param name="operation">The operation (e.g., "CardDetail", "TransactionList").</param>
    /// <param name="requestPayload">The request payload as JSON.</param>
    /// <param name="correlationId">Correlation ID for tracing.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>The mainframe response as a JSON string.</returns>
    Task<string> SendAsync(
        string domain,
        string operation,
        string requestPayload,
        string correlationId,
        CancellationToken cancellationToken = default);
}
