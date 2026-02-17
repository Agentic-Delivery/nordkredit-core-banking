using NordKredit.Domain.Lending;

namespace NordKredit.UnitTests.Lending;

/// <summary>
/// Tests for the LoanApplication entity.
/// COBOL source: Dedicated program not yet in repository.
/// Business rule: LND-BR-003 (loan origination and credit assessment).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8, AML 2017:11.
/// </summary>
public class LoanApplicationTests
{
    [Fact]
    public void LoanApplication_ShouldStoreAllFields()
    {
        var application = new LoanApplication
        {
            ApplicationId = "APP-00000001",
            CustomerId = "CUST-0001",
            RequestedAmount = 500000.00m,
            RequestedTermMonths = 360,
            LoanType = LoanType.TermLoan,
            Status = LoanApplicationStatus.Pending,
            ApplicantName = "ANNA BJÖRKQVIST",
            ApplicationDate = new DateTime(2025, 1, 10),
            AmlKycPassed = false,
            CreditAssessmentPassed = false,
            ApprovedCreditLimit = null,
            RejectionReason = null
        };

        Assert.Equal("APP-00000001", application.ApplicationId);
        Assert.Equal("CUST-0001", application.CustomerId);
        Assert.Equal(500000.00m, application.RequestedAmount);
        Assert.Equal(360, application.RequestedTermMonths);
        Assert.Equal(LoanType.TermLoan, application.LoanType);
        Assert.Equal(LoanApplicationStatus.Pending, application.Status);
        Assert.Equal("ANNA BJÖRKQVIST", application.ApplicantName);
        Assert.Equal(new DateTime(2025, 1, 10), application.ApplicationDate);
        Assert.False(application.AmlKycPassed);
        Assert.False(application.CreditAssessmentPassed);
        Assert.Null(application.ApprovedCreditLimit);
        Assert.Null(application.RejectionReason);
    }

    [Fact]
    public void LoanApplication_StringProperties_DefaultToEmpty()
    {
        var application = new LoanApplication();

        Assert.Equal(string.Empty, application.ApplicationId);
        Assert.Equal(string.Empty, application.CustomerId);
        Assert.Equal(string.Empty, application.ApplicantName);
    }

    [Fact]
    public void LoanApplication_Defaults_StatusIsPending()
    {
        var application = new LoanApplication();

        Assert.Equal(LoanApplicationStatus.Pending, application.Status);
    }

    [Fact]
    public void LoanApplication_ApplicantName_ShouldSupportSwedishCharacters()
    {
        var application = new LoanApplication { ApplicantName = "ÖRJAN ÄNGSTRÖM" };

        Assert.Contains("Ö", application.ApplicantName);
        Assert.Contains("Ä", application.ApplicantName);
    }

    [Fact]
    public void LoanApplication_RequestedAmount_PreservesDecimalPrecision()
    {
        var application = new LoanApplication { RequestedAmount = 9999999999.99m };

        Assert.Equal(9999999999.99m, application.RequestedAmount);
    }
}
