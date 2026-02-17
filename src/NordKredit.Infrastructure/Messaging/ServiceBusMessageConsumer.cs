using System.Text.Json;
using Azure.Messaging.ServiceBus;
using Microsoft.Extensions.Logging;
using NordKredit.Domain.Payments.Messaging;

namespace NordKredit.Infrastructure.Messaging;

/// <summary>
/// Consumes payment clearing messages from Azure Service Bus queues with dead-letter handling.
/// Replaces IBM MQ MQGET operations for Bankgirot and SWIFT integration.
///
/// COBOL source: Payment programs using MQ Series API calls (MQGET).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
/// </summary>
public sealed partial class ServiceBusMessageConsumer : IMessageConsumer, IAsyncDisposable
{
    private readonly ServiceBusClient _client;
    private readonly ILogger<ServiceBusMessageConsumer> _logger;
    private ServiceBusProcessor? _processor;

    public ServiceBusMessageConsumer(
        ServiceBusClient client,
        ILogger<ServiceBusMessageConsumer> logger)
    {
        _client = client;
        _logger = logger;
    }

    public async Task StartConsumingAsync<T>(
        string queueName,
        Func<T, string, CancellationToken, Task<bool>> handler,
        CancellationToken cancellationToken = default) where T : class
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(queueName);
        ArgumentNullException.ThrowIfNull(handler);

        var options = new ServiceBusProcessorOptions
        {
            AutoCompleteMessages = false,
            MaxConcurrentCalls = 1,
            MaxAutoLockRenewalDuration = TimeSpan.FromMinutes(5),
        };

        _processor = _client.CreateProcessor(queueName, options);

        _processor.ProcessMessageAsync += async args =>
        {
            var correlationId = args.Message.CorrelationId ?? string.Empty;
            LogMessageReceived(queueName, correlationId, args.Message.MessageId);

            try
            {
                var message = JsonSerializer.Deserialize<T>(args.Message.Body.ToArray());
                if (message is null)
                {
                    LogDeserializationFailed(queueName, args.Message.MessageId);
                    await args.DeadLetterMessageAsync(
                        args.Message,
                        "DeserializationFailure",
                        "Failed to deserialize message body",
                        args.CancellationToken).ConfigureAwait(false);
                    return;
                }

                var handled = await handler(message, correlationId, args.CancellationToken).ConfigureAwait(false);
                if (handled)
                {
                    await args.CompleteMessageAsync(args.Message, args.CancellationToken).ConfigureAwait(false);
                    LogMessageCompleted(queueName, correlationId);
                }
                else
                {
                    await args.AbandonMessageAsync(args.Message, cancellationToken: args.CancellationToken).ConfigureAwait(false);
                    LogMessageAbandoned(queueName, correlationId);
                }
            }
            catch (JsonException ex)
            {
                LogDeserializationError(queueName, args.Message.MessageId, ex);
                await args.DeadLetterMessageAsync(
                    args.Message,
                    "DeserializationError",
                    ex.Message,
                    args.CancellationToken).ConfigureAwait(false);
            }
        };

        _processor.ProcessErrorAsync += args =>
        {
            LogProcessingError(queueName, args.ErrorSource.ToString(), args.Exception);
            return Task.CompletedTask;
        };

        LogStartingConsumer(queueName);
        await _processor.StartProcessingAsync(cancellationToken).ConfigureAwait(false);
    }

    public async ValueTask DisposeAsync()
    {
        if (_processor is not null)
        {
            await _processor.DisposeAsync().ConfigureAwait(false);
        }

        await _client.DisposeAsync().ConfigureAwait(false);
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Starting consumer for queue {QueueName}")]
    private partial void LogStartingConsumer(string queueName);

    [LoggerMessage(Level = LogLevel.Information, Message = "Message received from {QueueName} with correlationId={CorrelationId}, messageId={MessageId}")]
    private partial void LogMessageReceived(string queueName, string correlationId, string messageId);

    [LoggerMessage(Level = LogLevel.Information, Message = "Message completed for {QueueName} with correlationId={CorrelationId}")]
    private partial void LogMessageCompleted(string queueName, string correlationId);

    [LoggerMessage(Level = LogLevel.Warning, Message = "Message abandoned for {QueueName} with correlationId={CorrelationId}")]
    private partial void LogMessageAbandoned(string queueName, string correlationId);

    [LoggerMessage(Level = LogLevel.Error, Message = "Failed to deserialize message from {QueueName}, messageId={MessageId}")]
    private partial void LogDeserializationFailed(string queueName, string messageId);

    [LoggerMessage(Level = LogLevel.Error, Message = "Deserialization error for message from {QueueName}, messageId={MessageId}")]
    private partial void LogDeserializationError(string queueName, string messageId, Exception ex);

    [LoggerMessage(Level = LogLevel.Error, Message = "Processing error on {QueueName}, source={ErrorSource}")]
    private partial void LogProcessingError(string queueName, string errorSource, Exception ex);
}
