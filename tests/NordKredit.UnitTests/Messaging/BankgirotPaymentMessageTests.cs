using System.Text.Json;
using NordKredit.Domain.Payments.Messaging;

namespace NordKredit.UnitTests.Messaging;

/// <summary>
/// Tests for <see cref="BankgirotPaymentMessage"/> serialization and deserialization.
/// Verifies ISO 20022 pain.001 field mapping for Bankgirot payment clearing.
///
/// COBOL source: Payment programs using IBM MQ for Bankgirot clearing.
/// Regulations: FFFS 2014:5 Ch. 8 (accurate records), PSD2 Art. 97 (SCA).
/// </summary>
public class BankgirotPaymentMessageTests
{
    [Fact]
    public void Serialize_ValidMessage_ProducesValidJson()
    {
        var message = CreateValidBankgirotMessage();

        var json = JsonSerializer.Serialize(message);

        Assert.NotNull(json);
        Assert.Contains("\"MessageId\"", json);
        Assert.Contains("\"DebtorIban\"", json);
        Assert.Contains("\"CreditorBankgiroNumber\"", json);
    }

    [Fact]
    public void Deserialize_ValidJson_ProducesCorrectMessage()
    {
        var original = CreateValidBankgirotMessage();
        var json = JsonSerializer.Serialize(original);

        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(original.MessageId, deserialized.MessageId);
        Assert.Equal(original.Amount, deserialized.Amount);
        Assert.Equal(original.Currency, deserialized.Currency);
        Assert.Equal(original.DebtorIban, deserialized.DebtorIban);
        Assert.Equal(original.CreditorBankgiroNumber, deserialized.CreditorBankgiroNumber);
    }

    [Fact]
    public void RoundTrip_AllFields_PreservesValues()
    {
        var original = CreateValidBankgirotMessage();

        var json = JsonSerializer.Serialize(original);
        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(original.MessageId, deserialized.MessageId);
        Assert.Equal(original.CreationDateTime, deserialized.CreationDateTime);
        Assert.Equal(original.NumberOfTransactions, deserialized.NumberOfTransactions);
        Assert.Equal(original.ControlSum, deserialized.ControlSum);
        Assert.Equal(original.DebtorName, deserialized.DebtorName);
        Assert.Equal(original.DebtorIban, deserialized.DebtorIban);
        Assert.Equal(original.DebtorAgentBic, deserialized.DebtorAgentBic);
        Assert.Equal(original.CreditorName, deserialized.CreditorName);
        Assert.Equal(original.CreditorBankgiroNumber, deserialized.CreditorBankgiroNumber);
        Assert.Equal(original.CreditorAgentBic, deserialized.CreditorAgentBic);
        Assert.Equal(original.Amount, deserialized.Amount);
        Assert.Equal(original.Currency, deserialized.Currency);
        Assert.Equal(original.RequestedExecutionDate, deserialized.RequestedExecutionDate);
        Assert.Equal(original.PaymentReference, deserialized.PaymentReference);
        Assert.Equal(original.EndToEndId, deserialized.EndToEndId);
    }

    [Fact]
    public void RoundTrip_NullOptionalFields_PreservesNulls()
    {
        var original = CreateValidBankgirotMessage() with { PaymentReference = null };

        var json = JsonSerializer.Serialize(original);
        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Null(deserialized.PaymentReference);
    }

    [Fact]
    public void Serialize_DecimalAmount_PreservesPrecision()
    {
        // COBOL: PIC S9(13)V99 — must preserve 2 decimal places
        var message = CreateValidBankgirotMessage() with { Amount = 12345.67m, ControlSum = 12345.67m };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(12345.67m, deserialized.Amount);
        Assert.Equal(12345.67m, deserialized.ControlSum);
    }

    [Fact]
    public void Serialize_SwedishIban_PreservesFormat()
    {
        // Swedish IBANs: SE + 2 check digits + 20 digits
        var message = CreateValidBankgirotMessage() with { DebtorIban = "SE3550000000054910000003" };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal("SE3550000000054910000003", deserialized.DebtorIban);
    }

    [Fact]
    public void Serialize_BankgiroNumber_PreservesFormat()
    {
        // Bankgiro numbers: 7-8 digits
        var message = CreateValidBankgirotMessage() with { CreditorBankgiroNumber = "12345678" };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal("12345678", deserialized.CreditorBankgiroNumber);
    }

    [Fact]
    public void Deserialize_Utf8Bytes_ProducesCorrectMessage()
    {
        var original = CreateValidBankgirotMessage();
        var bytes = JsonSerializer.SerializeToUtf8Bytes(original);

        var deserialized = JsonSerializer.Deserialize<BankgirotPaymentMessage>(bytes);

        Assert.NotNull(deserialized);
        Assert.Equal(original.MessageId, deserialized.MessageId);
        Assert.Equal(original.Amount, deserialized.Amount);
    }

    private static BankgirotPaymentMessage CreateValidBankgirotMessage() => new()
    {
        MessageId = "MSG-2026-001",
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
        EndToEndId = "E2E-2026-001",
    };
}
