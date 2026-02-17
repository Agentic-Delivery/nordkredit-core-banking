using System.Text.Json;
using NordKredit.Domain.Payments.Messaging;

namespace NordKredit.UnitTests.Messaging;

/// <summary>
/// Tests for <see cref="SwiftPaymentMessage"/> serialization and deserialization.
/// Verifies SWIFT MT103 (Single Customer Credit Transfer) field mapping.
///
/// COBOL source: Payment programs using IBM MQ for SWIFT messaging.
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records), AML/KYC screening.
/// </summary>
public class SwiftPaymentMessageTests
{
    [Fact]
    public void Serialize_ValidMessage_ProducesValidJson()
    {
        var message = CreateValidSwiftMessage();

        var json = JsonSerializer.Serialize(message);

        Assert.NotNull(json);
        Assert.Contains("\"SenderReference\"", json);
        Assert.Contains("\"BankOperationCode\"", json);
        Assert.Contains("\"BeneficiaryInstitutionBic\"", json);
    }

    [Fact]
    public void Deserialize_ValidJson_ProducesCorrectMessage()
    {
        var original = CreateValidSwiftMessage();
        var json = JsonSerializer.Serialize(original);

        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(original.SenderReference, deserialized.SenderReference);
        Assert.Equal(original.Amount, deserialized.Amount);
        Assert.Equal(original.Currency, deserialized.Currency);
        Assert.Equal(original.BeneficiaryName, deserialized.BeneficiaryName);
    }

    [Fact]
    public void RoundTrip_AllFields_PreservesValues()
    {
        var original = CreateValidSwiftMessage();

        var json = JsonSerializer.Serialize(original);
        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(original.SenderReference, deserialized.SenderReference);
        Assert.Equal(original.BankOperationCode, deserialized.BankOperationCode);
        Assert.Equal(original.ValueDate, deserialized.ValueDate);
        Assert.Equal(original.Currency, deserialized.Currency);
        Assert.Equal(original.Amount, deserialized.Amount);
        Assert.Equal(original.OrderingCustomerName, deserialized.OrderingCustomerName);
        Assert.Equal(original.OrderingCustomerAccount, deserialized.OrderingCustomerAccount);
        Assert.Equal(original.OrderingInstitutionBic, deserialized.OrderingInstitutionBic);
        Assert.Equal(original.BeneficiaryName, deserialized.BeneficiaryName);
        Assert.Equal(original.BeneficiaryAccount, deserialized.BeneficiaryAccount);
        Assert.Equal(original.BeneficiaryInstitutionBic, deserialized.BeneficiaryInstitutionBic);
        Assert.Equal(original.RemittanceInformation, deserialized.RemittanceInformation);
        Assert.Equal(original.DetailsOfCharges, deserialized.DetailsOfCharges);
        Assert.Equal(original.InstructedAmount, deserialized.InstructedAmount);
        Assert.Equal(original.InstructedCurrency, deserialized.InstructedCurrency);
    }

    [Fact]
    public void RoundTrip_NullOptionalFields_PreservesNulls()
    {
        var original = CreateValidSwiftMessage() with
        {
            RemittanceInformation = null,
            InstructedAmount = null,
            InstructedCurrency = null,
        };

        var json = JsonSerializer.Serialize(original);
        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Null(deserialized.RemittanceInformation);
        Assert.Null(deserialized.InstructedAmount);
        Assert.Null(deserialized.InstructedCurrency);
    }

    [Fact]
    public void Serialize_DecimalAmount_PreservesPrecision()
    {
        // COBOL: PIC S9(13)V99 — must preserve 2 decimal places
        var message = CreateValidSwiftMessage() with { Amount = 25000.50m };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(25000.50m, deserialized.Amount);
    }

    [Fact]
    public void Serialize_MT103OperationCode_PreservesValue()
    {
        // MT103 field 23B: Bank operation code (CRED = credit transfer)
        var message = CreateValidSwiftMessage() with { BankOperationCode = "CRED" };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal("CRED", deserialized.BankOperationCode);
    }

    [Fact]
    public void Serialize_DetailsOfCharges_PreservesValue()
    {
        // MT103 field 71A: SHA (shared), OUR (sender pays), BEN (beneficiary pays)
        var message = CreateValidSwiftMessage() with { DetailsOfCharges = "SHA" };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal("SHA", deserialized.DetailsOfCharges);
    }

    [Fact]
    public void Serialize_InstructedAmountDifferentCurrency_PreservesValues()
    {
        // MT103 field 33B: instructed amount in different currency
        var message = CreateValidSwiftMessage() with
        {
            InstructedAmount = 2500.00m,
            InstructedCurrency = "SEK",
        };

        var json = JsonSerializer.Serialize(message);
        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(json);

        Assert.NotNull(deserialized);
        Assert.Equal(2500.00m, deserialized.InstructedAmount);
        Assert.Equal("SEK", deserialized.InstructedCurrency);
    }

    [Fact]
    public void Deserialize_Utf8Bytes_ProducesCorrectMessage()
    {
        var original = CreateValidSwiftMessage();
        var bytes = JsonSerializer.SerializeToUtf8Bytes(original);

        var deserialized = JsonSerializer.Deserialize<SwiftPaymentMessage>(bytes);

        Assert.NotNull(deserialized);
        Assert.Equal(original.SenderReference, deserialized.SenderReference);
        Assert.Equal(original.Amount, deserialized.Amount);
    }

    private static SwiftPaymentMessage CreateValidSwiftMessage() => new()
    {
        SenderReference = "REF-2026-MT103-001",
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
        RemittanceInformation = "Invoice 2026-001",
        DetailsOfCharges = "SHA",
        InstructedAmount = 25000.00m,
        InstructedCurrency = "EUR",
    };
}
