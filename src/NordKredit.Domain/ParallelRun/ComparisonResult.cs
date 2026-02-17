namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Result of comparing mainframe and Azure system outputs for a single operation.
/// Used during parallel-run validation to track functional equivalence.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class ComparisonResult
{
    /// <summary>Whether the outputs match (after applying known-difference rules).</summary>
    public required bool IsMatch { get; init; }

    /// <summary>The domain being compared (e.g., "CardManagement", "Transactions").</summary>
    public required string Domain { get; init; }

    /// <summary>The operation being compared (e.g., "CardDetail", "TransactionList").</summary>
    public required string Operation { get; init; }

    /// <summary>Field-level divergences found (empty if IsMatch is true).</summary>
    public required IReadOnlyList<FieldDivergence> Divergences { get; init; }

    /// <summary>Timestamp of the comparison.</summary>
    public required DateTimeOffset ComparedAt { get; init; }

    /// <summary>Correlation ID for end-to-end tracing and audit trail.</summary>
    public required string CorrelationId { get; init; }
}

/// <summary>
/// A single field-level divergence between mainframe and Azure outputs.
/// </summary>
public class FieldDivergence
{
    /// <summary>The field name that diverged (e.g., "amount", "cardNumber").</summary>
    public required string FieldName { get; init; }

    /// <summary>The value from the mainframe system.</summary>
    public required string? MainframeValue { get; init; }

    /// <summary>The value from the Azure system.</summary>
    public required string? AzureValue { get; init; }

    /// <summary>Category of the divergence for metrics grouping.</summary>
    public required DivergenceCategory Category { get; init; }
}

/// <summary>
/// Categories for divergence tracking and metrics.
/// </summary>
public enum DivergenceCategory
{
    /// <summary>Data value mismatch (e.g., different amount, wrong account).</summary>
    DataMismatch,

    /// <summary>Field missing in one system's output.</summary>
    MissingField,

    /// <summary>Format difference (e.g., date format, number precision).</summary>
    FormatDifference,

    /// <summary>Encoding difference (e.g., EBCDIC vs Unicode character mapping).</summary>
    EncodingDifference
}
