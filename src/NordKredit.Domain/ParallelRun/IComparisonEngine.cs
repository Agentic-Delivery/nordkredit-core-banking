namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Compares mainframe and Azure outputs, applying known-difference rules.
/// Known differences (card masking, date formats, EBCDIC encoding) are excluded from divergence.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public interface IComparisonEngine
{
    /// <summary>
    /// Compares two JSON responses and returns a detailed comparison result.
    /// Known intentional differences are filtered out based on domain-specific rules.
    /// </summary>
    /// <param name="domain">The domain (e.g., "CardManagement", "Transactions").</param>
    /// <param name="operation">The operation (e.g., "CardDetail", "TransactionList").</param>
    /// <param name="mainframeResponse">The mainframe response as JSON.</param>
    /// <param name="azureResponse">The Azure system response as JSON.</param>
    /// <param name="correlationId">Correlation ID for tracing.</param>
    /// <returns>Comparison result with match status and field-level divergences.</returns>
    ComparisonResult Compare(
        string domain,
        string operation,
        string mainframeResponse,
        string azureResponse,
        string correlationId);
}
