using NordKredit.Domain.Lending;

namespace NordKredit.UnitTests.Lending;

/// <summary>
/// Tests for lending input validation.
/// COBOL source: CBTRN02C.cbl:370-421 (1500-VALIDATE-TRAN), COCRDLIC.cbl:1003-1034.
/// Business rules: LND-BR-002 (credit limit enforcement), LND-BR-009 (expiration enforcement).
/// Regulations: FSA FFFS 2014:5 Ch. 6, PSD2 Art. 64.
/// </summary>
public class LoanValidationServiceTests
{
    // =================================================================
    // Account ID validation
    // =================================================================

    [Fact]
    public void ValidateAccountId_Valid11DigitNumber_Succeeds()
    {
        var result = LoanValidationService.ValidateAccountId("12345678901");

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_BlankInput_ReturnsError()
    {
        var result = LoanValidationService.ValidateAccountId("");

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_NullInput_ReturnsError()
    {
        var result = LoanValidationService.ValidateAccountId(null);

        Assert.False(result.IsValid);
        Assert.Equal("Account number not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_NonNumeric_ReturnsError()
    {
        var result = LoanValidationService.ValidateAccountId("ABCDEFGHIJK");

        Assert.False(result.IsValid);
        Assert.Contains("11 DIGIT NUMBER", result.ErrorMessage);
    }

    [Fact]
    public void ValidateAccountId_AllZeros_ReturnsError()
    {
        var result = LoanValidationService.ValidateAccountId("00000000000");

        Assert.False(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_TooShort_ReturnsError()
    {
        var result = LoanValidationService.ValidateAccountId("1234567890");

        Assert.False(result.IsValid);
    }

    [Fact]
    public void ValidateAccountId_TooLong_ReturnsError()
    {
        var result = LoanValidationService.ValidateAccountId("123456789012");

        Assert.False(result.IsValid);
    }

    // =================================================================
    // LND-BR-002: Credit limit validation
    // COBOL source: CBTRN02C.cbl:403-413
    // =================================================================

    [Fact]
    public void ValidateCreditLimit_PositiveAmount_Succeeds()
    {
        var result = LoanValidationService.ValidateCreditLimit(50000.00m);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateCreditLimit_ZeroAmount_ReturnsError()
    {
        var result = LoanValidationService.ValidateCreditLimit(0m);

        Assert.False(result.IsValid);
        Assert.Contains("must be greater than zero", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCreditLimit_NegativeAmount_ReturnsError()
    {
        var result = LoanValidationService.ValidateCreditLimit(-1000.00m);

        Assert.False(result.IsValid);
        Assert.Contains("must be greater than zero", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCreditLimit_ExceedsMaxCobolValue_ReturnsError()
    {
        // COBOL: PIC S9(10)V99 — max 9,999,999,999.99
        var result = LoanValidationService.ValidateCreditLimit(10000000000.00m);

        Assert.False(result.IsValid);
        Assert.Contains("exceeds maximum", result.ErrorMessage);
    }

    [Fact]
    public void ValidateCreditLimit_AtMaxCobolValue_Succeeds()
    {
        var result = LoanValidationService.ValidateCreditLimit(9999999999.99m);

        Assert.True(result.IsValid);
    }

    // =================================================================
    // Transaction amount validation
    // =================================================================

    [Fact]
    public void ValidateTransactionAmount_PositiveAmount_Succeeds()
    {
        var result = LoanValidationService.ValidateTransactionAmount(1500.50m);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateTransactionAmount_NegativeRepayment_Succeeds()
    {
        var result = LoanValidationService.ValidateTransactionAmount(-3000.00m);

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateTransactionAmount_ZeroAmount_ReturnsError()
    {
        var result = LoanValidationService.ValidateTransactionAmount(0m);

        Assert.False(result.IsValid);
        Assert.Contains("cannot be zero", result.ErrorMessage);
    }

    // =================================================================
    // LND-BR-008: Loan status transition validation
    // =================================================================

    [Theory]
    [InlineData(LoanStatus.Active, LoanStatus.Delinquent)]
    [InlineData(LoanStatus.Active, LoanStatus.Frozen)]
    [InlineData(LoanStatus.Active, LoanStatus.PaidOff)]
    [InlineData(LoanStatus.Active, LoanStatus.Closed)]
    [InlineData(LoanStatus.Delinquent, LoanStatus.Active)]
    [InlineData(LoanStatus.Delinquent, LoanStatus.Defaulted)]
    [InlineData(LoanStatus.Delinquent, LoanStatus.Frozen)]
    [InlineData(LoanStatus.Frozen, LoanStatus.Active)]
    [InlineData(LoanStatus.Frozen, LoanStatus.Delinquent)]
    [InlineData(LoanStatus.Frozen, LoanStatus.Closed)]
    [InlineData(LoanStatus.Defaulted, LoanStatus.Closed)]
    [InlineData(LoanStatus.PaidOff, LoanStatus.Closed)]
    public void ValidateStatusTransition_ValidTransitions_Succeed(
        LoanStatus current, LoanStatus target)
    {
        var result = LoanValidationService.ValidateStatusTransition(current, target);

        Assert.True(result.IsValid);
    }

    [Theory]
    [InlineData(LoanStatus.Closed, LoanStatus.Active)]
    [InlineData(LoanStatus.Closed, LoanStatus.Delinquent)]
    [InlineData(LoanStatus.Closed, LoanStatus.Frozen)]
    public void ValidateStatusTransition_FromClosed_Fails(
        LoanStatus current, LoanStatus target)
    {
        var result = LoanValidationService.ValidateStatusTransition(current, target);

        Assert.False(result.IsValid);
        Assert.Contains("Cannot transition from Closed status", result.ErrorMessage);
    }

    [Theory]
    [InlineData(LoanStatus.Active)]
    [InlineData(LoanStatus.Delinquent)]
    [InlineData(LoanStatus.Frozen)]
    [InlineData(LoanStatus.Defaulted)]
    [InlineData(LoanStatus.PaidOff)]
    [InlineData(LoanStatus.Closed)]
    public void ValidateStatusTransition_SameStatus_Fails(LoanStatus status)
    {
        var result = LoanValidationService.ValidateStatusTransition(status, status);

        Assert.False(result.IsValid);
        Assert.Contains("already in the requested status", result.ErrorMessage);
    }

    [Theory]
    [InlineData(LoanStatus.Active, LoanStatus.Defaulted)]
    [InlineData(LoanStatus.PaidOff, LoanStatus.Active)]
    [InlineData(LoanStatus.PaidOff, LoanStatus.Delinquent)]
    [InlineData(LoanStatus.Defaulted, LoanStatus.Active)]
    public void ValidateStatusTransition_InvalidTransitions_Fail(
        LoanStatus current, LoanStatus target)
    {
        var result = LoanValidationService.ValidateStatusTransition(current, target);

        Assert.False(result.IsValid);
    }
}
