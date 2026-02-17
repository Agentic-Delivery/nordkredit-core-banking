using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.Lending;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for lending operations.
/// Replaces COBOL CICS screens for loan management, application processing, and payment schedules.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), CBTRN02C.cbl (transaction processing).
/// Business rules: LND-BR-001 through LND-BR-009.
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Consumer Credit Directive 2008/48/EC,
///              PSD2 Art. 64, GDPR Art. 5/15, AML 2017:11.
/// SEC-BR-001: Internal only — requires OperatorAccess (Azure AD).
/// </summary>
[ApiController]
[Route("api/loans")]
[Authorize(Policy = "OperatorAccess")]
public class LendingController : ControllerBase
{
    private readonly ILoanRepository _loanRepository;
    private readonly ILoanApplicationRepository _applicationRepository;

    public LendingController(
        ILoanRepository loanRepository,
        ILoanApplicationRepository applicationRepository)
    {
        _loanRepository = loanRepository;
        _applicationRepository = applicationRepository;
    }

    /// <summary>
    /// Lists loans with keyset pagination.
    /// COBOL: Sequential read with STARTBR/READNEXT pattern on ACCTFILE.
    /// Business rule: LND-BR-001 (data structure).
    /// Regulations: FSA FFFS 2014:5 Ch. 6, GDPR Art. 15.
    /// </summary>
    [HttpGet]
    public async Task<IActionResult> ListLoans(
        [FromQuery] string? afterAccountId,
        CancellationToken cancellationToken)
    {
        if (!string.IsNullOrEmpty(afterAccountId))
        {
            var validation = LoanValidationService.ValidateAccountId(afterAccountId);
            if (!validation.IsValid)
            {
                return BadRequest(new { Message = validation.ErrorMessage });
            }
        }

        var loans = await _loanRepository.GetPageAsync(7, afterAccountId, cancellationToken);
        return Ok(loans.Select(MapToLoanResponse));
    }

    /// <summary>
    /// Retrieves a single loan by account ID (status inquiry).
    /// COBOL: READ ACCOUNT-FILE using FD-ACCT-ID.
    /// Business rules: LND-BR-001 (data structure), LND-BR-002 (credit limit).
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, GDPR Art. 15.
    /// </summary>
    [HttpGet("{accountId}")]
    public async Task<IActionResult> GetLoan(
        string accountId,
        CancellationToken cancellationToken)
    {
        var validation = LoanValidationService.ValidateAccountId(accountId);
        if (!validation.IsValid)
        {
            return BadRequest(new { Message = validation.ErrorMessage });
        }

        var loan = await _loanRepository.GetByAccountIdAsync(accountId, cancellationToken);
        if (loan is null)
        {
            return NotFound(new { Message = "Loan not found" });
        }

        return Ok(MapToLoanResponse(loan));
    }

    /// <summary>
    /// Creates a new loan account.
    /// COBOL: WRITE ACCOUNT-RECORD to ACCTFILE.
    /// Business rules: LND-BR-001 (data structure), LND-BR-002 (credit limit), LND-BR-003 (origination).
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, AML 2017:11.
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> CreateLoan(
        [FromBody] CreateLoanRequest request,
        CancellationToken cancellationToken)
    {
        var idValidation = LoanValidationService.ValidateAccountId(request.AccountId);
        if (!idValidation.IsValid)
        {
            return BadRequest(new { Message = idValidation.ErrorMessage });
        }

        var limitValidation = LoanValidationService.ValidateCreditLimit(request.CreditLimit);
        if (!limitValidation.IsValid)
        {
            return BadRequest(new { Message = limitValidation.ErrorMessage });
        }

        var existing = await _loanRepository.GetByAccountIdAsync(request.AccountId, cancellationToken);
        if (existing is not null)
        {
            return Conflict(new { Message = "Loan account already exists" });
        }

        var loan = new Loan
        {
            AccountId = request.AccountId,
            ActiveStatus = LoanStatus.Active,
            LoanType = request.LoanType,
            CreditLimit = request.CreditLimit,
            CashCreditLimit = request.CashCreditLimit,
            BorrowerName = request.BorrowerName,
            DisclosureGroupId = request.DisclosureGroupId,
            OriginationDate = DateTime.UtcNow
        };

        await _loanRepository.AddAsync(loan, cancellationToken);

        return CreatedAtAction(nameof(GetLoan), new { accountId = loan.AccountId }, MapToLoanResponse(loan));
    }

    /// <summary>
    /// Updates loan status (lifecycle transition).
    /// Business rule: LND-BR-008 (delinquency management, status state machine).
    /// State machine: Active → Delinquent, Frozen, PaidOff, Closed; etc.
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Inkassolagen 1974:182.
    /// </summary>
    [HttpPut("{accountId}/status")]
    public async Task<IActionResult> UpdateLoanStatus(
        string accountId,
        [FromBody] UpdateLoanStatusRequest request,
        CancellationToken cancellationToken)
    {
        var ifMatchValues = Request?.Headers.IfMatch;
        var ifMatch = ifMatchValues?.ToString();
        if (string.IsNullOrEmpty(ifMatch))
        {
            return StatusCode(428, new { Message = "ETag required for update" });
        }

        var idValidation = LoanValidationService.ValidateAccountId(accountId);
        if (!idValidation.IsValid)
        {
            return BadRequest(new { Message = idValidation.ErrorMessage });
        }

        byte[] rowVersion;
        try
        {
            rowVersion = Convert.FromBase64String(ifMatch);
        }
        catch (FormatException)
        {
            return BadRequest(new { Message = "Invalid ETag format" });
        }

        var loan = await _loanRepository.GetByAccountIdAsync(accountId, cancellationToken);
        if (loan is null)
        {
            return NotFound(new { Message = "Loan not found" });
        }

        if (!loan.RowVersion.SequenceEqual(rowVersion))
        {
            return Conflict(new { Message = "Loan was modified by another user", Loan = MapToLoanResponse(loan) });
        }

        var transitionResult = loan.TransitionTo(request.Status);
        if (!transitionResult.IsValid)
        {
            return BadRequest(new { Message = transitionResult.ErrorMessage });
        }

        await _loanRepository.UpdateAsync(loan, cancellationToken);

        return Ok(MapToLoanResponse(loan));
    }

    /// <summary>
    /// Generates an amortization (payment) schedule for a loan.
    /// COBOL source: Dedicated program not yet in repository.
    /// Business rule: LND-BR-004 (interest calculation and amortization schedule).
    /// Regulations: Consumer Credit Directive Art. 10 (credit agreement information),
    ///              FSA FFFS 2014:5 Ch. 6.
    /// </summary>
    [HttpGet("{accountId}/schedule")]
    public async Task<IActionResult> GetPaymentSchedule(
        string accountId,
        [FromQuery] decimal annualRate,
        [FromQuery] int termMonths,
        CancellationToken cancellationToken)
    {
        var idValidation = LoanValidationService.ValidateAccountId(accountId);
        if (!idValidation.IsValid)
        {
            return BadRequest(new { Message = idValidation.ErrorMessage });
        }

        if (termMonths <= 0)
        {
            return BadRequest(new { Message = "Term months must be greater than zero" });
        }

        var loan = await _loanRepository.GetByAccountIdAsync(accountId, cancellationToken);
        if (loan is null)
        {
            return NotFound(new { Message = "Loan not found" });
        }

        var schedule = InterestCalculationService.GenerateAmortizationSchedule(
            loan.CurrentBalance, annualRate, termMonths, DateTime.UtcNow);

        var monthlyPayment = termMonths > 0 && loan.CurrentBalance > 0
            ? InterestCalculationService.CalculateMonthlyPayment(loan.CurrentBalance, annualRate, termMonths)
            : 0m;

        return Ok(new PaymentScheduleResponse
        {
            AccountId = loan.AccountId,
            Principal = loan.CurrentBalance,
            AnnualRate = annualRate,
            TermMonths = termMonths,
            MonthlyPayment = monthlyPayment,
            Schedule = schedule
        });
    }

    /// <summary>
    /// Calculates early repayment amount including accrued interest.
    /// Business rule: LND-BR-005 (repayment processing), LND-BR-004 (interest calculation).
    /// Regulations: Consumer Credit Directive Art. 16 (early repayment right),
    ///              Swedish Consumer Credit Act (konsumentkreditlagen 2010:1846) §36.
    /// </summary>
    [HttpGet("{accountId}/early-repayment")]
    public async Task<IActionResult> CalculateEarlyRepayment(
        string accountId,
        [FromQuery] decimal annualRate,
        [FromQuery] int remainingMonths,
        CancellationToken cancellationToken)
    {
        var idValidation = LoanValidationService.ValidateAccountId(accountId);
        if (!idValidation.IsValid)
        {
            return BadRequest(new { Message = idValidation.ErrorMessage });
        }

        var loan = await _loanRepository.GetByAccountIdAsync(accountId, cancellationToken);
        if (loan is null)
        {
            return NotFound(new { Message = "Loan not found" });
        }

        // Accrued interest for the current period (30 days, Actual/360 convention)
        var accruedInterest = InterestCalculationService.CalculateDailyInterest(
            loan.CurrentBalance, annualRate, DayCountConvention.Actual360, 30);

        var totalRepaymentAmount = loan.CurrentBalance + accruedInterest;

        return Ok(new EarlyRepaymentResponse
        {
            AccountId = loan.AccountId,
            OutstandingPrincipal = loan.CurrentBalance,
            AnnualRate = annualRate,
            AccruedInterest = accruedInterest,
            TotalRepaymentAmount = totalRepaymentAmount,
            RemainingMonths = remainingMonths
        });
    }

    /// <summary>
    /// Submits a new loan application.
    /// Business rule: LND-BR-003 (loan origination and credit assessment).
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8,
    ///              AML 2017:11 (customer due diligence).
    /// </summary>
    [HttpPost("/api/loan-applications")]
    public async Task<IActionResult> SubmitApplication(
        [FromBody] SubmitLoanApplicationRequest request,
        CancellationToken cancellationToken)
    {
        if (string.IsNullOrWhiteSpace(request.CustomerId))
        {
            return BadRequest(new { Message = "Customer ID is required" });
        }

        if (request.RequestedAmount <= 0)
        {
            return BadRequest(new { Message = "Requested amount must be greater than zero" });
        }

        var application = new LoanApplication
        {
            ApplicationId = $"APP-{DateTime.UtcNow:yyyyMMddHHmmssfff}",
            CustomerId = request.CustomerId,
            ApplicantName = request.ApplicantName,
            RequestedAmount = request.RequestedAmount,
            RequestedTermMonths = request.RequestedTermMonths,
            LoanType = request.LoanType,
            Status = LoanApplicationStatus.Pending,
            ApplicationDate = DateTime.UtcNow
        };

        await _applicationRepository.AddAsync(application, cancellationToken);

        return CreatedAtAction(nameof(GetApplication),
            new { applicationId = application.ApplicationId },
            MapToApplicationResponse(application));
    }

    /// <summary>
    /// Retrieves a single loan application by ID.
    /// Business rule: LND-BR-003.
    /// Regulations: GDPR Art. 15, Consumer Credit Directive Art. 9.
    /// </summary>
    [HttpGet("/api/loan-applications/{applicationId}")]
    public async Task<IActionResult> GetApplication(
        string applicationId,
        CancellationToken cancellationToken)
    {
        if (string.IsNullOrWhiteSpace(applicationId))
        {
            return BadRequest(new { Message = "Application ID cannot be empty" });
        }

        var application = await _applicationRepository.GetByIdAsync(applicationId, cancellationToken);
        if (application is null)
        {
            return NotFound(new { Message = "Loan application not found" });
        }

        return Ok(MapToApplicationResponse(application));
    }

    /// <summary>
    /// Lists loan applications for a customer.
    /// Business rule: LND-BR-003.
    /// Regulations: GDPR Art. 15, Consumer Credit Directive Art. 9.
    /// </summary>
    [HttpGet("/api/loan-applications")]
    public async Task<IActionResult> ListApplications(
        [FromQuery] string customerId,
        CancellationToken cancellationToken)
    {
        if (string.IsNullOrWhiteSpace(customerId))
        {
            return BadRequest(new { Message = "Customer ID is required" });
        }

        var applications = await _applicationRepository.GetByCustomerIdAsync(customerId, cancellationToken);
        return Ok(applications.Select(MapToApplicationResponse));
    }

    private static LoanResponse MapToLoanResponse(Loan loan) => new()
    {
        AccountId = loan.AccountId,
        Status = loan.ActiveStatus.ToString(),
        LoanType = loan.LoanType.ToString(),
        CurrentBalance = loan.CurrentBalance,
        CreditLimit = loan.CreditLimit,
        CashCreditLimit = loan.CashCreditLimit,
        AvailableCredit = loan.AvailableCredit,
        CurrentCycleCredit = loan.CurrentCycleCredit,
        CurrentCycleDebit = loan.CurrentCycleDebit,
        ExpirationDate = loan.ExpirationDate?.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        BorrowerName = loan.BorrowerName,
        DisclosureGroupId = loan.DisclosureGroupId,
        OriginationDate = loan.OriginationDate.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        ETag = Convert.ToBase64String(loan.RowVersion)
    };

    private static LoanApplicationResponse MapToApplicationResponse(LoanApplication app) => new()
    {
        ApplicationId = app.ApplicationId,
        CustomerId = app.CustomerId,
        ApplicantName = app.ApplicantName,
        RequestedAmount = app.RequestedAmount,
        RequestedTermMonths = app.RequestedTermMonths,
        LoanType = app.LoanType.ToString(),
        Status = app.Status.ToString(),
        ApplicationDate = app.ApplicationDate.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        AmlKycPassed = app.AmlKycPassed,
        CreditAssessmentPassed = app.CreditAssessmentPassed,
        ApprovedCreditLimit = app.ApprovedCreditLimit,
        RejectionReason = app.RejectionReason
    };
}

public class CreateLoanRequest
{
    public string AccountId { get; set; } = string.Empty;
    public LoanType LoanType { get; set; }
    public decimal CreditLimit { get; set; }
    public decimal CashCreditLimit { get; set; }
    public string BorrowerName { get; set; } = string.Empty;
    public string DisclosureGroupId { get; set; } = string.Empty;
}

public class UpdateLoanStatusRequest
{
    public LoanStatus Status { get; set; }
}

public class SubmitLoanApplicationRequest
{
    public string CustomerId { get; set; } = string.Empty;
    public string ApplicantName { get; set; } = string.Empty;
    public decimal RequestedAmount { get; set; }
    public int RequestedTermMonths { get; set; }
    public LoanType LoanType { get; set; }
}

public class LoanResponse
{
    public string AccountId { get; set; } = string.Empty;
    public string Status { get; set; } = string.Empty;
    public string LoanType { get; set; } = string.Empty;
    public decimal CurrentBalance { get; set; }
    public decimal CreditLimit { get; set; }
    public decimal CashCreditLimit { get; set; }
    public decimal AvailableCredit { get; set; }
    public decimal CurrentCycleCredit { get; set; }
    public decimal CurrentCycleDebit { get; set; }
    public string? ExpirationDate { get; set; }
    public string BorrowerName { get; set; } = string.Empty;
    public string DisclosureGroupId { get; set; } = string.Empty;
    public string OriginationDate { get; set; } = string.Empty;
    public string ETag { get; set; } = string.Empty;
}

public class LoanApplicationResponse
{
    public string ApplicationId { get; set; } = string.Empty;
    public string CustomerId { get; set; } = string.Empty;
    public string ApplicantName { get; set; } = string.Empty;
    public decimal RequestedAmount { get; set; }
    public int RequestedTermMonths { get; set; }
    public string LoanType { get; set; } = string.Empty;
    public string Status { get; set; } = string.Empty;
    public string ApplicationDate { get; set; } = string.Empty;
    public bool AmlKycPassed { get; set; }
    public bool CreditAssessmentPassed { get; set; }
    public decimal? ApprovedCreditLimit { get; set; }
    public string? RejectionReason { get; set; }
}

public class PaymentScheduleResponse
{
    public string AccountId { get; set; } = string.Empty;
    public decimal Principal { get; set; }
    public decimal AnnualRate { get; set; }
    public int TermMonths { get; set; }
    public decimal MonthlyPayment { get; set; }
    public IReadOnlyList<AmortizationEntry> Schedule { get; set; } = [];
}

public class EarlyRepaymentResponse
{
    public string AccountId { get; set; } = string.Empty;
    public decimal OutstandingPrincipal { get; set; }
    public decimal AnnualRate { get; set; }
    public decimal AccruedInterest { get; set; }
    public decimal TotalRepaymentAmount { get; set; }
    public int RemainingMonths { get; set; }
}
