using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the DepositValidationService.
/// COBOL source: CVACT01Y.cpy — account field validation.
/// Business rule: DEP-BR-001 (deposit account data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97.
/// </summary>
public class DepositValidationServiceTests
{
    // =================================================================
    // Account ID validation
    // =================================================================

    [Fact]
    public void ValidateAccountId_ValidId_ReturnsSuccess()
    {
        var result = DepositValidationService.ValidateAccountId("12345678901");

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_Null_ReturnsError()
    {
        var result = DepositValidationService.ValidateAccountId(null);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_Empty_ReturnsError()
    {
        var result = DepositValidationService.ValidateAccountId("");

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_TooShort_ReturnsError()
    {
        var result = DepositValidationService.ValidateAccountId("12345");

        Assert.False(result.IsValid);
        Assert.Equal("Account number must be an 11-digit number", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_TooLong_ReturnsError()
    {
        var result = DepositValidationService.ValidateAccountId("123456789012");

        Assert.False(result.IsValid);
        Assert.Equal("Account number must be an 11-digit number", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_NonNumeric_ReturnsError()
    {
        var result = DepositValidationService.ValidateAccountId("1234567890A");

        Assert.False(result.IsValid);
        Assert.Equal("Account number must be an 11-digit number", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_AllZeros_ReturnsError()
    {
        var result = DepositValidationService.ValidateAccountId("00000000000");

        Assert.False(result.IsValid);
        Assert.Equal("Account number must be an 11-digit number", result.ErrorMessage);
    }

    // =================================================================
    // Status transition validation
    // =================================================================

    [Theory]
    [InlineData(DepositAccountStatus.Active, DepositAccountStatus.Dormant)]
    [InlineData(DepositAccountStatus.Active, DepositAccountStatus.Frozen)]
    [InlineData(DepositAccountStatus.Active, DepositAccountStatus.Closed)]
    [InlineData(DepositAccountStatus.Dormant, DepositAccountStatus.Active)]
    [InlineData(DepositAccountStatus.Dormant, DepositAccountStatus.Frozen)]
    [InlineData(DepositAccountStatus.Dormant, DepositAccountStatus.Closed)]
    [InlineData(DepositAccountStatus.Frozen, DepositAccountStatus.Active)]
    [InlineData(DepositAccountStatus.Frozen, DepositAccountStatus.Dormant)]
    [InlineData(DepositAccountStatus.Frozen, DepositAccountStatus.Closed)]
    public void ValidateStatusTransition_ValidTransitions_ReturnSuccess(
        DepositAccountStatus current,
        DepositAccountStatus target)
    {
        var result = DepositValidationService.ValidateStatusTransition(current, target);

        Assert.True(result.IsValid);
    }

    [Theory]
    [InlineData(DepositAccountStatus.Closed, DepositAccountStatus.Active)]
    [InlineData(DepositAccountStatus.Closed, DepositAccountStatus.Dormant)]
    [InlineData(DepositAccountStatus.Closed, DepositAccountStatus.Frozen)]
    public void ValidateStatusTransition_FromClosed_ReturnsError(
        DepositAccountStatus current,
        DepositAccountStatus target)
    {
        var result = DepositValidationService.ValidateStatusTransition(current, target);

        Assert.False(result.IsValid);
        Assert.Equal("Cannot transition from Closed status — terminal state", result.ErrorMessage);
    }

    [Theory]
    [InlineData(DepositAccountStatus.Active)]
    [InlineData(DepositAccountStatus.Dormant)]
    [InlineData(DepositAccountStatus.Frozen)]
    [InlineData(DepositAccountStatus.Closed)]
    public void ValidateStatusTransition_SameStatus_ReturnsError(DepositAccountStatus status)
    {
        var result = DepositValidationService.ValidateStatusTransition(status, status);

        Assert.False(result.IsValid);
        Assert.Equal("Account is already in the requested status", result.ErrorMessage);
    }

    // =================================================================
    // Disclosure group ID validation
    // =================================================================

    [Fact]
    public void ValidateDisclosureGroupId_ValidId_ReturnsSuccess()
    {
        var result = DepositValidationService.ValidateDisclosureGroupId("SAV-STD");

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateDisclosureGroupId_Null_ReturnsError()
    {
        var result = DepositValidationService.ValidateDisclosureGroupId(null);

        Assert.False(result.IsValid);
        Assert.Equal("Disclosure group ID not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateDisclosureGroupId_Empty_ReturnsError()
    {
        var result = DepositValidationService.ValidateDisclosureGroupId("");

        Assert.False(result.IsValid);
        Assert.Equal("Disclosure group ID not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateDisclosureGroupId_TooLong_ReturnsError()
    {
        var result = DepositValidationService.ValidateDisclosureGroupId("12345678901");

        Assert.False(result.IsValid);
        Assert.Equal("Disclosure group ID must not exceed 10 characters", result.ErrorMessage);
    }

    [Fact]
    public void ValidateDisclosureGroupId_ExactlyMaxLength_ReturnsSuccess()
    {
        var result = DepositValidationService.ValidateDisclosureGroupId("1234567890");

        Assert.True(result.IsValid);
    }
}
