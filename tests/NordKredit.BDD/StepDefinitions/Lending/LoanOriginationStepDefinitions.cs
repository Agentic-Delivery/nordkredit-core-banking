using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for loan origination and credit assessment BDD scenarios (LND-BR-003).
/// COBOL source: Dedicated program not yet in repository (inferred from CVACT01Y.cpy).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8,
///              AML 2017:11 (customer due diligence), GDPR Art. 6(1)(b).
/// </summary>
[Binding]
[Scope(Feature = "Loan origination and credit assessment")]
public sealed class LoanOriginationStepDefinitions
{
    private LoanApplication _application = null!;

    [When(@"I submit a loan application for customer ""(.*)"" requesting (.+) for (\d+) months as (.+)")]
    public void WhenISubmitALoanApplication(string customerId, decimal amount, int termMonths, string loanType)
    {
        _application = new LoanApplication
        {
            ApplicationId = "APP-001",
            CustomerId = customerId,
            RequestedAmount = amount,
            RequestedTermMonths = termMonths,
            LoanType = Enum.Parse<LoanType>(loanType),
            ApplicationDate = DateTime.UtcNow
        };
    }

    [Given(@"a loan application for customer ""(.*)"" requesting (.+)")]
    public void GivenALoanApplicationForCustomerRequesting(string customerId, decimal amount)
    {
        _application = new LoanApplication
        {
            ApplicationId = "APP-001",
            CustomerId = customerId,
            RequestedAmount = amount,
            RequestedTermMonths = 360,
            LoanType = LoanType.Mortgage,
            ApplicationDate = DateTime.UtcNow
        };
    }

    [When(@"the application is approved with credit limit (.+)")]
    public void WhenTheApplicationIsApprovedWithCreditLimit(decimal creditLimit)
    {
        _application.Status = LoanApplicationStatus.Approved;
        _application.AmlKycPassed = true;
        _application.CreditAssessmentPassed = true;
        _application.ApprovedCreditLimit = creditLimit;
    }

    [When(@"the application is rejected with reason ""(.*)""")]
    public void WhenTheApplicationIsRejectedWithReason(string reason)
    {
        _application.Status = LoanApplicationStatus.Rejected;
        _application.RejectionReason = reason;
    }

    [When(@"the application is cancelled")]
    public void WhenTheApplicationIsCancelled() =>
        _application.Status = LoanApplicationStatus.Cancelled;

    [Then(@"the application status is ""(.*)""")]
    public void ThenTheApplicationStatusIs(string expectedStatus) =>
        Assert.Equal(Enum.Parse<LoanApplicationStatus>(expectedStatus), _application.Status);

    [Then(@"AML/KYC has not been verified")]
    public void ThenAmlKycHasNotBeenVerified() =>
        Assert.False(_application.AmlKycPassed);

    [Then(@"credit assessment has not been performed")]
    public void ThenCreditAssessmentHasNotBeenPerformed() =>
        Assert.False(_application.CreditAssessmentPassed);

    [Then(@"the approved credit limit is (.+)")]
    public void ThenTheApprovedCreditLimitIs(decimal expected) =>
        Assert.Equal(expected, _application.ApprovedCreditLimit);

    [Then(@"the rejection reason is ""(.*)""")]
    public void ThenTheRejectionReasonIs(string expectedReason) =>
        Assert.Equal(expectedReason, _application.RejectionReason);
}
