namespace NordKredit.Domain.Payments.Messaging;

/// <summary>
/// Consumes payment clearing messages from queues with dead-letter handling.
/// Replaces IBM MQ MQGET operations used for Bankgirot and SWIFT integration.
///
/// COBOL source: Various payment programs using MQ Series API calls.
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
/// </summary>
public interface IMessageConsumer
{
    /// <summary>
    /// Receives and processes messages from the specified queue.
    /// Messages that fail processing are moved to the dead-letter queue after max retries.
    /// </summary>
    /// <typeparam name="T">The message payload type to deserialize.</typeparam>
    /// <param name="queueName">The source queue name.</param>
    /// <param name="handler">Async handler invoked for each received message. Return true to complete, false to abandon.</param>
    /// <param name="cancellationToken">Cancellation token to stop consuming.</param>
    Task StartConsumingAsync<T>(
        string queueName,
        Func<T, string, CancellationToken, Task<bool>> handler,
        CancellationToken cancellationToken = default) where T : class;
}
