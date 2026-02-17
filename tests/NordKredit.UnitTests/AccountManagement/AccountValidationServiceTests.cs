using NordKredit.Domain.AccountManagement;

namespace NordKredit.UnitTests.AccountManagement;

/// <summary>
/// Tests for account input validation.
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760.
/// Business rule: ACCT-BR-002 (account ID input validation).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97.
/// </summary>
public class AccountValidationServiceTests
{
    // =================================================================
    // ACCT-BR-002: Account ID input validation
    // =================================================================

    [Fact]
    public void ValidateAccountId_Valid11DigitNumber_Succeeds()
    {
        var result = AccountValidationService.ValidateAccountId("12345678901", required: true);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_BlankWhenRequired_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_NullWhenRequired_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId(null, required: true);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_BlankWhenOptional_Succeeds()
    {
        var result = AccountValidationService.ValidateAccountId("", required: false);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_NullWhenOptional_Succeeds()
    {
        var result = AccountValidationService.ValidateAccountId(null, required: false);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_NonNumeric_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("ABCDEFGHIJK", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_AllZeros_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("00000000000", required: true);

        Assert.False(result.IsValid);
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_MixedAlphanumeric_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("1234567890A", required: true);

        Assert.False(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_TooShort_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("1234567890", required: true);

        Assert.False(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_TooLong_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("123456789012", required: true);

        Assert.False(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_WithSpaces_ReturnsError()
    {
        var result = AccountValidationService.ValidateAccountId("123 4567890", required: true);

        Assert.False(result.IsValid);
    }

    // =================================================================
    // ACCT-BR-005: Status transition validation
    // =================================================================

    [Theory]
    [InlineData(AccountStatus.Active, AccountStatus.Dormant)]
    [InlineData(AccountStatus.Active, AccountStatus.Frozen)]
    [InlineData(AccountStatus.Active, AccountStatus.Closed)]
    [InlineData(AccountStatus.Dormant, AccountStatus.Active)]
    [InlineData(AccountStatus.Dormant, AccountStatus.Frozen)]
    [InlineData(AccountStatus.Dormant, AccountStatus.Closed)]
    [InlineData(AccountStatus.Frozen, AccountStatus.Active)]
    [InlineData(AccountStatus.Frozen, AccountStatus.Dormant)]
    [InlineData(AccountStatus.Frozen, AccountStatus.Closed)]
    public void ValidateStatusTransition_ValidTransitions_Succeed(
        AccountStatus current, AccountStatus target)
    {
        var result = AccountValidationService.ValidateStatusTransition(current, target);

        Assert.True(result.IsValid);
    }

    [Theory]
    [InlineData(AccountStatus.Closed, AccountStatus.Active)]
    [InlineData(AccountStatus.Closed, AccountStatus.Dormant)]
    [InlineData(AccountStatus.Closed, AccountStatus.Frozen)]
    public void ValidateStatusTransition_FromClosed_Fails(
        AccountStatus current, AccountStatus target)
    {
        var result = AccountValidationService.ValidateStatusTransition(current, target);

        Assert.False(result.IsValid);
        Assert.Contains("Cannot transition from Closed status", result.ErrorMessage);
    }

    [Theory]
    [InlineData(AccountStatus.Active)]
    [InlineData(AccountStatus.Dormant)]
    [InlineData(AccountStatus.Frozen)]
    [InlineData(AccountStatus.Closed)]
    public void ValidateStatusTransition_SameStatus_Fails(AccountStatus status)
    {
        var result = AccountValidationService.ValidateStatusTransition(status, status);

        Assert.False(result.IsValid);
        Assert.Contains("already in the requested status", result.ErrorMessage);
    }
}
