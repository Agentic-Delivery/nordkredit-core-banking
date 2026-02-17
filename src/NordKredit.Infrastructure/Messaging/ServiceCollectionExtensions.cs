using Azure.Messaging.ServiceBus;
using Microsoft.Extensions.DependencyInjection;
using NordKredit.Domain.Payments.Messaging;

namespace NordKredit.Infrastructure.Messaging;

/// <summary>
/// DI registration for Azure Service Bus messaging infrastructure.
/// Configures retry policies with exponential backoff (max 3 retries)
/// as required for reliable payment clearing message delivery.
/// </summary>
public static class ServiceCollectionExtensions
{
    /// <summary>
    /// Registers Azure Service Bus messaging services for payment clearing.
    /// Replaces IBM MQ connectivity for Bankgirot and SWIFT integration.
    /// </summary>
    /// <param name="services">The service collection.</param>
    /// <param name="connectionString">Azure Service Bus connection string.</param>
    /// <returns>The service collection for chaining.</returns>
    public static IServiceCollection AddServiceBusMessaging(
        this IServiceCollection services,
        string connectionString)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(connectionString);

        services.AddSingleton(_ => new ServiceBusClient(
            connectionString,
            new ServiceBusClientOptions
            {
                TransportType = ServiceBusTransportType.AmqpTcp,
                RetryOptions = new ServiceBusRetryOptions
                {
                    Mode = ServiceBusRetryMode.Exponential,
                    MaxRetries = 3,
                    Delay = TimeSpan.FromSeconds(1),
                    MaxDelay = TimeSpan.FromSeconds(30),
                    TryTimeout = TimeSpan.FromSeconds(60),
                },
            }));

        services.AddSingleton<IMessagePublisher, ServiceBusMessagePublisher>();
        services.AddSingleton<IMessageConsumer, ServiceBusMessageConsumer>();

        return services;
    }
}
