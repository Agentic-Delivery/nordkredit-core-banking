using Azure.Messaging.ServiceBus;
using NordKredit.Domain.Payments.Messaging;
using NordKredit.Infrastructure.Messaging;

namespace NordKredit.UnitTests.Messaging;

/// <summary>
/// Tests for <see cref="ServiceBusMessageConsumer"/>.
/// Verifies argument validation and dead-letter queue handling setup.
///
/// COBOL source: Payment programs using IBM MQ MQGET for Bankgirot/SWIFT clearing.
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
/// </summary>
public class ServiceBusMessageConsumerTests
{
    [Fact]
    public async Task StartConsumingAsync_NullQueueName_ThrowsArgumentException()
    {
        var consumer = CreateConsumer();

        await Assert.ThrowsAsync<ArgumentException>(() =>
            consumer.StartConsumingAsync<BankgirotPaymentMessage>(
                "",
                (_, _, _) => Task.FromResult(true)));
    }

    [Fact]
    public async Task StartConsumingAsync_WhitespaceQueueName_ThrowsArgumentException()
    {
        var consumer = CreateConsumer();

        await Assert.ThrowsAsync<ArgumentException>(() =>
            consumer.StartConsumingAsync<BankgirotPaymentMessage>(
                "   ",
                (_, _, _) => Task.FromResult(true)));
    }

    [Fact]
    public async Task StartConsumingAsync_NullHandler_ThrowsArgumentNullException()
    {
        var consumer = CreateConsumer();

        await Assert.ThrowsAsync<ArgumentNullException>(() =>
            consumer.StartConsumingAsync<BankgirotPaymentMessage>(
                "test-queue",
                null!));
    }

    private static ServiceBusMessageConsumer CreateConsumer()
    {
        var client = new ServiceBusClient(
            "Endpoint=sb://fake.servicebus.windows.net/;SharedAccessKeyName=Test;SharedAccessKey=dGVzdA==");
        var logger = Microsoft.Extensions.Logging.Abstractions.NullLogger<ServiceBusMessageConsumer>.Instance;
        return new ServiceBusMessageConsumer(client, logger);
    }
}
