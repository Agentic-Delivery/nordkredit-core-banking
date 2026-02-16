namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Thrown when a concurrent modification is detected during card update.
/// COBOL source: COCRDUPC.cbl:1498-1523 — 9300-CHECK-CHANGE-IN-REC.
/// Maps DATA-WAS-CHANGED-BEFORE-UPDATE to a typed exception caught by the service layer.
/// In the .NET implementation, this maps to DbUpdateConcurrencyException from EF Core
/// when the SQL rowversion column detects a concurrent modification.
/// Business rule: CARD-BR-008.
/// Regulations: FFFS 2014:5 Ch. 8 §4.
/// </summary>
public class ConcurrencyConflictException : Exception
{
    public ConcurrencyConflictException()
        : base("Record changed by some one else. Please review")
    {
    }

    public ConcurrencyConflictException(string message)
        : base(message)
    {
    }

    public ConcurrencyConflictException(string message, Exception innerException)
        : base(message, innerException)
    {
    }
}
