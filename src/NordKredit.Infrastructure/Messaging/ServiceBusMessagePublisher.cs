using System.Text.Json;
using Azure.Messaging.ServiceBus;
using Microsoft.Extensions.Logging;
using NordKredit.Domain.Payments.Messaging;

namespace NordKredit.Infrastructure.Messaging;

/// <summary>
/// Publishes payment clearing messages to Azure Service Bus queues/topics.
/// Replaces IBM MQ MQPUT operations for Bankgirot and SWIFT integration.
///
/// COBOL source: Payment programs using MQ Series API calls (MQPUT).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
/// </summary>
public sealed partial class ServiceBusMessagePublisher : IMessagePublisher, IAsyncDisposable
{
    private readonly ServiceBusClient _client;
    private readonly ILogger<ServiceBusMessagePublisher> _logger;

    public ServiceBusMessagePublisher(
        ServiceBusClient client,
        ILogger<ServiceBusMessagePublisher> logger)
    {
        _client = client;
        _logger = logger;
    }

    public async Task SendAsync<T>(
        string queueOrTopicName,
        T message,
        string correlationId,
        CancellationToken cancellationToken = default) where T : class
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(queueOrTopicName);
        ArgumentNullException.ThrowIfNull(message);
        ArgumentException.ThrowIfNullOrWhiteSpace(correlationId);

        var sender = _client.CreateSender(queueOrTopicName);
        await using (sender.ConfigureAwait(false))
        {
            var body = JsonSerializer.SerializeToUtf8Bytes(message);
            var serviceBusMessage = new ServiceBusMessage(body)
            {
                ContentType = "application/json",
                CorrelationId = correlationId,
                MessageId = Guid.NewGuid().ToString(),
                Subject = typeof(T).Name,
            };

            LogSendingMessage(queueOrTopicName, correlationId, typeof(T).Name);
            await sender.SendMessageAsync(serviceBusMessage, cancellationToken).ConfigureAwait(false);
            LogMessageSent(queueOrTopicName, correlationId);
        }
    }

    public async ValueTask DisposeAsync() =>
        await _client.DisposeAsync().ConfigureAwait(false);

    [LoggerMessage(Level = LogLevel.Information, Message = "Sending message to {QueueOrTopic} with correlationId={CorrelationId}, type={MessageType}")]
    private partial void LogSendingMessage(string queueOrTopic, string correlationId, string messageType);

    [LoggerMessage(Level = LogLevel.Information, Message = "Message sent to {QueueOrTopic} with correlationId={CorrelationId}")]
    private partial void LogMessageSent(string queueOrTopic, string correlationId);
}
