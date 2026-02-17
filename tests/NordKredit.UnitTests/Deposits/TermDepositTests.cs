using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the TermDeposit entity.
/// Business rule: DEP-BR-005 (term deposit maturity and renewal processing).
/// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive, PSD2 Art. 57.
/// </summary>
public class TermDepositTests
{
    [Fact]
    public void TermDeposit_ShouldStoreAllFields()
    {
        var termDeposit = new TermDeposit
        {
            AccountId = "12345678901",
            TermMonths = 12,
            FixedRate = 0.035m,
            PrincipalAmount = 100000.00m,
            RenewalInstruction = RenewalInstruction.AutoRenew,
            LinkedAccountId = "98765432101",
            StartDate = new DateTime(2025, 1, 15),
            RenewalCount = 2
        };

        Assert.Equal("12345678901", termDeposit.AccountId);
        Assert.Equal(12, termDeposit.TermMonths);
        Assert.Equal(0.035m, termDeposit.FixedRate);
        Assert.Equal(100000.00m, termDeposit.PrincipalAmount);
        Assert.Equal(RenewalInstruction.AutoRenew, termDeposit.RenewalInstruction);
        Assert.Equal("98765432101", termDeposit.LinkedAccountId);
        Assert.Equal(new DateTime(2025, 1, 15), termDeposit.StartDate);
        Assert.Equal(2, termDeposit.RenewalCount);
    }

    [Fact]
    public void TermDeposit_StringProperties_DefaultToEmpty()
    {
        var termDeposit = new TermDeposit();

        Assert.Equal(string.Empty, termDeposit.AccountId);
        Assert.Null(termDeposit.LinkedAccountId);
    }

    [Fact]
    public void TermDeposit_Defaults_RenewalInstructionIsHold()
    {
        var termDeposit = new TermDeposit();

        Assert.Equal(RenewalInstruction.Hold, termDeposit.RenewalInstruction);
    }

    [Fact]
    public void TermDeposit_FinancialFields_UseDecimal()
    {
        // COBOL: PIC S9(10)V99 — must use decimal, never float/double
        var termDeposit = new TermDeposit
        {
            FixedRate = 0.0350m,
            PrincipalAmount = 9999999999.99m
        };

        Assert.Equal(0.0350m, termDeposit.FixedRate);
        Assert.Equal(9999999999.99m, termDeposit.PrincipalAmount);
    }

    [Theory]
    [InlineData(RenewalInstruction.AutoRenew)]
    [InlineData(RenewalInstruction.Payout)]
    [InlineData(RenewalInstruction.Hold)]
    public void TermDeposit_SupportsAllRenewalInstructions(RenewalInstruction instruction)
    {
        var termDeposit = new TermDeposit { RenewalInstruction = instruction };

        Assert.Equal(instruction, termDeposit.RenewalInstruction);
    }

    [Fact]
    public void TermDeposit_PayoutInstruction_RequiresLinkedAccount()
    {
        var termDeposit = new TermDeposit
        {
            RenewalInstruction = RenewalInstruction.Payout,
            LinkedAccountId = "98765432101"
        };

        Assert.Equal(RenewalInstruction.Payout, termDeposit.RenewalInstruction);
        Assert.Equal("98765432101", termDeposit.LinkedAccountId);
    }
}
