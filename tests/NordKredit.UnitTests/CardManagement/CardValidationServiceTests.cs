using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardValidationService.
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-724, COCRDUPC.cbl:721-800.
/// Covers account number validation (CARD-BR-004) and card number validation (CARD-BR-005).
/// Regulations: FFFS 2014:5 Ch. 4 §3 (operational risk), PSD2 Art. 97.
/// </summary>
public class CardValidationServiceTests
{
    // ===================================================================
    // Account Number Validation — CARD-BR-004
    // COBOL: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760
    // ===================================================================

    [Fact]
    public void ValidateAccountNumber_ValidElevenDigits_Passes()
    {
        var result = CardValidationService.ValidateAccountNumber("12345678901", required: true);

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_ValidWithLeadingZeros_Passes()
    {
        var result = CardValidationService.ValidateAccountNumber("00012345678", required: true);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountNumber_Null_Required_ReturnsNotProvided()
    {
        var result = CardValidationService.ValidateAccountNumber(null, required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_Empty_Required_ReturnsNotProvided()
    {
        var result = CardValidationService.ValidateAccountNumber("", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_Whitespace_Required_ReturnsNotProvided()
    {
        var result = CardValidationService.ValidateAccountNumber("           ", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_Null_NotRequired_Passes()
    {
        var result = CardValidationService.ValidateAccountNumber(null, required: false);

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_Empty_NotRequired_Passes()
    {
        var result = CardValidationService.ValidateAccountNumber("", required: false);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountNumber_Whitespace_NotRequired_Passes()
    {
        var result = CardValidationService.ValidateAccountNumber("   ", required: false);

        Assert.True(result.IsValid);
    }

    [Theory]
    [InlineData("ABCDEFGHIJK")]
    [InlineData("ABC12345678")]
    [InlineData("1234567890A")]
    [InlineData("12345 67890")]
    public void ValidateAccountNumber_NonNumeric_ReturnsFormatError(string input)
    {
        var result = CardValidationService.ValidateAccountNumber(input, required: true);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_AllZeros_ReturnsFormatError()
    {
        var result = CardValidationService.ValidateAccountNumber("00000000000", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_TooShort_ReturnsFormatError()
    {
        // In COBOL, shorter input is padded with spaces, making it non-numeric
        var result = CardValidationService.ValidateAccountNumber("12345", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_TooLong_ReturnsFormatError()
    {
        var result = CardValidationService.ValidateAccountNumber("123456789012", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountNumber_NonNumeric_NotRequired_ReturnsFormatError()
    {
        // Even when not required, if provided it must be valid
        var result = CardValidationService.ValidateAccountNumber("ABCDEFGHIJK", required: false);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    // ===================================================================
    // Card Number Validation — CARD-BR-005
    // COBOL: COCRDSLC.cbl:685-724, COCRDUPC.cbl:762-800
    // ===================================================================

    [Fact]
    public void ValidateCardNumber_ValidSixteenDigits_Passes()
    {
        var result = CardValidationService.ValidateCardNumber("4000123456789012", required: true);

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_ValidWithLeadingZeros_Passes()
    {
        var result = CardValidationService.ValidateCardNumber("0000123456789012", required: true);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateCardNumber_Null_Required_ReturnsNotProvided()
    {
        var result = CardValidationService.ValidateCardNumber(null, required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Card number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_Empty_Required_ReturnsNotProvided()
    {
        var result = CardValidationService.ValidateCardNumber("", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Card number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_Whitespace_Required_ReturnsNotProvided()
    {
        var result = CardValidationService.ValidateCardNumber("                ", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Card number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_AllZeros_Required_ReturnsNotProvided()
    {
        // COBOL treats all-zeros same as blank for card number
        var result = CardValidationService.ValidateCardNumber("0000000000000000", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Card number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_Null_NotRequired_Passes()
    {
        var result = CardValidationService.ValidateCardNumber(null, required: false);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateCardNumber_Empty_NotRequired_Passes()
    {
        var result = CardValidationService.ValidateCardNumber("", required: false);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateCardNumber_AllZeros_NotRequired_Passes()
    {
        // When not required, all-zeros is treated as blank (skip filter)
        var result = CardValidationService.ValidateCardNumber("0000000000000000", required: false);

        Assert.True(result.IsValid);
    }

    [Theory]
    [InlineData("ABCD123456789012")]
    [InlineData("400012345678901A")]
    [InlineData("40001234 5678901")]
    public void ValidateCardNumber_NonNumeric_ReturnsFormatError(string input)
    {
        var result = CardValidationService.ValidateCardNumber(input, required: true);

        Assert.False(result.IsValid);
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_TooShort_ReturnsFormatError()
    {
        var result = CardValidationService.ValidateCardNumber("1234567890", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_TooLong_ReturnsFormatError()
    {
        var result = CardValidationService.ValidateCardNumber("12345678901234567", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCardNumber_NonNumeric_NotRequired_ReturnsFormatError()
    {
        // Even when not required, if provided it must be valid
        var result = CardValidationService.ValidateCardNumber("ABCD123456789012", required: false);

        Assert.False(result.IsValid);
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", result.ErrorMessage);
    }
}
