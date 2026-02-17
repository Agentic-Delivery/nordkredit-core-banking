namespace NordKredit.Domain.Payments.Messaging;

/// <summary>
/// Publishes payment clearing messages to named queues or topics.
/// Replaces IBM MQ MQPUT operations used for Bankgirot and SWIFT integration.
///
/// COBOL source: Various payment programs using MQ Series API calls.
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
/// </summary>
public interface IMessagePublisher
{
    /// <summary>
    /// Sends a message to the specified queue or topic with a correlation ID for traceability.
    /// </summary>
    /// <typeparam name="T">The message payload type (e.g., BankgirotPaymentMessage, SwiftPaymentMessage).</typeparam>
    /// <param name="queueOrTopicName">The destination queue or topic name.</param>
    /// <param name="message">The message payload.</param>
    /// <param name="correlationId">Correlation ID for end-to-end tracing and regulatory audit trails.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    Task SendAsync<T>(
        string queueOrTopicName,
        T message,
        string correlationId,
        CancellationToken cancellationToken = default) where T : class;
}
