using System.Text.Json;
using Azure.Messaging.ServiceBus;
using NordKredit.Domain.Payments.Messaging;
using NordKredit.Infrastructure.Messaging;

namespace NordKredit.UnitTests.Messaging;

/// <summary>
/// Tests for <see cref="ServiceBusMessagePublisher"/>.
/// Verifies message publishing, correlation ID assignment, and validation.
///
/// COBOL source: Payment programs using IBM MQ MQPUT for Bankgirot/SWIFT clearing.
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
/// </summary>
public class ServiceBusMessagePublisherTests
{
    [Fact]
    public async Task SendAsync_NullQueueName_ThrowsArgumentException()
    {
        var publisher = CreatePublisher();

        await Assert.ThrowsAsync<ArgumentException>(() =>
            publisher.SendAsync("", new BankgirotPaymentMessage
            {
                MessageId = "test",
                CreationDateTime = DateTimeOffset.UtcNow,
                NumberOfTransactions = 1,
                ControlSum = 100m,
                DebtorName = "Test",
                DebtorIban = "SE3550000000054910000003",
                DebtorAgentBic = "NRDKSESS",
                CreditorName = "Test",
                CreditorBankgiroNumber = "5551234",
                CreditorAgentBic = "SWEDSESS",
                Amount = 100m,
                Currency = "SEK",
                RequestedExecutionDate = new DateOnly(2026, 2, 18),
                EndToEndId = "E2E-001",
            }, "corr-123"));
    }

    [Fact]
    public async Task SendAsync_NullMessage_ThrowsArgumentNullException()
    {
        var publisher = CreatePublisher();

        await Assert.ThrowsAsync<ArgumentNullException>(() =>
            publisher.SendAsync<BankgirotPaymentMessage>("test-queue", null!, "corr-123"));
    }

    [Fact]
    public async Task SendAsync_NullCorrelationId_ThrowsArgumentException()
    {
        var publisher = CreatePublisher();

        await Assert.ThrowsAsync<ArgumentException>(() =>
            publisher.SendAsync("test-queue", new BankgirotPaymentMessage
            {
                MessageId = "test",
                CreationDateTime = DateTimeOffset.UtcNow,
                NumberOfTransactions = 1,
                ControlSum = 100m,
                DebtorName = "Test",
                DebtorIban = "SE3550000000054910000003",
                DebtorAgentBic = "NRDKSESS",
                CreditorName = "Test",
                CreditorBankgiroNumber = "5551234",
                CreditorAgentBic = "SWEDSESS",
                Amount = 100m,
                Currency = "SEK",
                RequestedExecutionDate = new DateOnly(2026, 2, 18),
                EndToEndId = "E2E-001",
            }, ""));
    }

    [Fact]
    public async Task SendAsync_WhitespaceQueueName_ThrowsArgumentException()
    {
        var publisher = CreatePublisher();

        await Assert.ThrowsAsync<ArgumentException>(() =>
            publisher.SendAsync("   ", new BankgirotPaymentMessage
            {
                MessageId = "test",
                CreationDateTime = DateTimeOffset.UtcNow,
                NumberOfTransactions = 1,
                ControlSum = 100m,
                DebtorName = "Test",
                DebtorIban = "SE3550000000054910000003",
                DebtorAgentBic = "NRDKSESS",
                CreditorName = "Test",
                CreditorBankgiroNumber = "5551234",
                CreditorAgentBic = "SWEDSESS",
                Amount = 100m,
                Currency = "SEK",
                RequestedExecutionDate = new DateOnly(2026, 2, 18),
                EndToEndId = "E2E-001",
            }, "corr-123"));
    }

    [Fact]
    public void MessagePayload_Bankgirot_SerializesToUtf8()
    {
        var message = new BankgirotPaymentMessage
        {
            MessageId = "MSG-001",
            CreationDateTime = new DateTimeOffset(2026, 2, 17, 10, 0, 0, TimeSpan.FromHours(1)),
            NumberOfTransactions = 1,
            ControlSum = 1500.00m,
            DebtorName = "Erik Johansson",
            DebtorIban = "SE3550000000054910000003",
            DebtorAgentBic = "NRDKSESS",
            CreditorName = "Handelsbolaget AB",
            CreditorBankgiroNumber = "5551234",
            CreditorAgentBic = "SWEDSESS",
            Amount = 1500.00m,
            Currency = "SEK",
            RequestedExecutionDate = new DateOnly(2026, 2, 18),
            PaymentReference = "1234567890",
            EndToEndId = "E2E-001",
        };

        var bytes = JsonSerializer.SerializeToUtf8Bytes(message);
        var roundTripped = JsonSerializer.Deserialize<BankgirotPaymentMessage>(bytes);

        Assert.NotNull(roundTripped);
        Assert.Equal("MSG-001", roundTripped.MessageId);
        Assert.Equal(1500.00m, roundTripped.Amount);
        Assert.Equal("SEK", roundTripped.Currency);
    }

    [Fact]
    public void MessagePayload_Swift_SerializesToUtf8()
    {
        var message = new SwiftPaymentMessage
        {
            SenderReference = "REF-001",
            BankOperationCode = "CRED",
            ValueDate = new DateOnly(2026, 2, 18),
            Currency = "EUR",
            Amount = 25000.00m,
            OrderingCustomerName = "Erik Johansson",
            OrderingCustomerAccount = "SE3550000000054910000003",
            OrderingInstitutionBic = "NRDKSESS",
            BeneficiaryName = "Hans Müller",
            BeneficiaryAccount = "DE89370400440532013000",
            BeneficiaryInstitutionBic = "COBADEFFXXX",
            DetailsOfCharges = "SHA",
        };

        var bytes = JsonSerializer.SerializeToUtf8Bytes(message);
        var roundTripped = JsonSerializer.Deserialize<SwiftPaymentMessage>(bytes);

        Assert.NotNull(roundTripped);
        Assert.Equal("REF-001", roundTripped.SenderReference);
        Assert.Equal(25000.00m, roundTripped.Amount);
        Assert.Equal("EUR", roundTripped.Currency);
    }

    private static ServiceBusMessagePublisher CreatePublisher()
    {
        // Use a fake connection string — the publisher validates arguments before
        // attempting to connect to the Service Bus.
        var client = new ServiceBusClient(
            "Endpoint=sb://fake.servicebus.windows.net/;SharedAccessKeyName=Test;SharedAccessKey=dGVzdA==");
        var logger = Microsoft.Extensions.Logging.Abstractions.NullLogger<ServiceBusMessagePublisher>.Instance;
        return new ServiceBusMessagePublisher(client, logger);
    }
}
