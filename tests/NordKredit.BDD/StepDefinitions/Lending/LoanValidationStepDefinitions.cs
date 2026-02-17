using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for loan input validation BDD scenarios (LND-BR-002, LND-BR-005, LND-BR-009).
/// COBOL source: CBTRN02C.cbl:370-421 (1500-VALIDATE-TRAN), COCRDLIC.cbl:1003-1034.
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), PSD2 Art. 64 (transaction integrity).
/// </summary>
[Binding]
[Scope(Feature = "Loan account and credit limit input validation")]
public sealed class LoanValidationStepDefinitions
{
    private LoanValidationResult _result = null!;

    [When(@"I validate loan account ID ""(.*)""")]
    public void WhenIValidateLoanAccountId(string accountId) =>
        _result = LoanValidationService.ValidateAccountId(accountId);

    [When(@"I validate credit limit (.+)")]
    public void WhenIValidateCreditLimit(decimal amount) =>
        _result = LoanValidationService.ValidateCreditLimit(amount);

    [When(@"I validate transaction amount (.+)")]
    public void WhenIValidateTransactionAmount(decimal amount) =>
        _result = LoanValidationService.ValidateTransactionAmount(amount);

    [Then(@"the loan validation result is valid")]
    public void ThenTheLoanValidationResultIsValid() =>
        Assert.True(_result.IsValid);

    [Then(@"the loan validation result is invalid")]
    public void ThenTheLoanValidationResultIsInvalid() =>
        Assert.False(_result.IsValid);

    [Then(@"the loan validation error is ""(.*)""")]
    public void ThenTheLoanValidationErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _result.ErrorMessage);
}
