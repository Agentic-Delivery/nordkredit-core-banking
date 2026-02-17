using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.Lending;

namespace NordKredit.UnitTests.Lending;

/// <summary>
/// Tests for the LendingController.
/// Business rules: LND-BR-001 through LND-BR-009.
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive 2008/48/EC,
///              PSD2 Art. 64, GDPR Art. 5/15, AML 2017:11.
/// </summary>
public class LendingControllerTests
{
    private readonly InMemoryLoanRepository _loanRepo = new();
    private readonly InMemoryLoanApplicationRepository _applicationRepo = new();

    private LendingController CreateController() =>
        new(_loanRepo, _applicationRepo)
        {
            ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            }
        };

    private LendingController CreateControllerWithIfMatch(string etagValue)
    {
        var controller = CreateController();
        controller.Request.Headers.IfMatch = etagValue;
        return controller;
    }

    // =================================================================
    // GET /api/loans — List loans
    // =================================================================

    [Fact]
    public async Task ListLoans_ReturnsOk()
    {
        // GIVEN loans exist
        await _loanRepo.AddAsync(CreateTestLoan("12345678901"));
        await _loanRepo.AddAsync(CreateTestLoan("12345678902"));

        // WHEN listing loans
        var controller = CreateController();
        var result = await controller.ListLoans(null, CancellationToken.None);

        // THEN returns OK with loans
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ListLoans_WithInvalidCursor_ReturnsBadRequest()
    {
        // GIVEN an invalid cursor value
        var controller = CreateController();

        // WHEN listing with invalid cursor
        var result = await controller.ListLoans("INVALID", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/loans/{accountId} — Loan detail (status inquiry)
    // =================================================================

    [Fact]
    public async Task GetLoan_ValidId_ReturnsOk()
    {
        // GIVEN a loan exists
        var loan = CreateTestLoan("12345678901");
        await _loanRepo.AddAsync(loan);

        // WHEN requesting by account ID
        var controller = CreateController();
        var result = await controller.GetLoan("12345678901", CancellationToken.None);

        // THEN returns OK with loan details
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<LoanResponse>(okResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal("Active", response.Status);
    }

    [Fact]
    public async Task GetLoan_InvalidId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();

        // WHEN requesting with invalid ID
        var result = await controller.GetLoan("INVALID", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task GetLoan_NotFound_ReturnsNotFound()
    {
        // GIVEN no loan exists for the ID
        var controller = CreateController();

        // WHEN requesting a non-existent loan
        var result = await controller.GetLoan("99999999999", CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    // =================================================================
    // POST /api/loans — Create loan
    // =================================================================

    [Fact]
    public async Task CreateLoan_ValidRequest_ReturnsCreated()
    {
        // GIVEN a valid loan creation request
        var controller = CreateController();
        var request = new CreateLoanRequest
        {
            AccountId = "12345678901",
            LoanType = LoanType.TermLoan,
            CreditLimit = 500000.00m,
            CashCreditLimit = 50000.00m,
            BorrowerName = "ANNA SVENSSON",
            DisclosureGroupId = "MORTGAGE_01"
        };

        // WHEN creating the loan
        var result = await controller.CreateLoan(request, CancellationToken.None);

        // THEN returns Created
        var createdResult = Assert.IsType<CreatedAtActionResult>(result);
        var response = Assert.IsType<LoanResponse>(createdResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal("Active", response.Status);
    }

    [Fact]
    public async Task CreateLoan_InvalidAccountId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();
        var request = new CreateLoanRequest
        {
            AccountId = "INVALID",
            BorrowerName = "TEST"
        };

        // WHEN creating the loan
        var result = await controller.CreateLoan(request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task CreateLoan_InvalidCreditLimit_ReturnsBadRequest()
    {
        // GIVEN a zero credit limit
        var controller = CreateController();
        var request = new CreateLoanRequest
        {
            AccountId = "12345678901",
            CreditLimit = 0m,
            BorrowerName = "TEST"
        };

        // WHEN creating the loan
        var result = await controller.CreateLoan(request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task CreateLoan_Duplicate_ReturnsConflict()
    {
        // GIVEN a loan already exists
        await _loanRepo.AddAsync(CreateTestLoan("12345678901"));

        var controller = CreateController();
        var request = new CreateLoanRequest
        {
            AccountId = "12345678901",
            CreditLimit = 500000.00m,
            BorrowerName = "TEST"
        };

        // WHEN creating a duplicate
        var result = await controller.CreateLoan(request, CancellationToken.None);

        // THEN returns Conflict
        Assert.IsType<ConflictObjectResult>(result);
    }

    // =================================================================
    // PUT /api/loans/{accountId}/status — Status transition
    // =================================================================

    [Fact]
    public async Task UpdateLoanStatus_ValidTransition_ReturnsOk()
    {
        // GIVEN an active loan
        var loan = CreateTestLoan("12345678901");
        loan.RowVersion = [0x01, 0x02, 0x03, 0x04];
        await _loanRepo.AddAsync(loan);

        var controller = CreateControllerWithIfMatch(
            Convert.ToBase64String([0x01, 0x02, 0x03, 0x04]));

        // WHEN transitioning to Frozen
        var request = new UpdateLoanStatusRequest { Status = LoanStatus.Frozen };
        var result = await controller.UpdateLoanStatus("12345678901", request, CancellationToken.None);

        // THEN returns OK with updated status
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<LoanResponse>(okResult.Value);
        Assert.Equal("Frozen", response.Status);
    }

    [Fact]
    public async Task UpdateLoanStatus_MissingETag_Returns428()
    {
        // GIVEN no ETag in request
        var controller = CreateController();
        var request = new UpdateLoanStatusRequest { Status = LoanStatus.Frozen };

        // WHEN updating status
        var result = await controller.UpdateLoanStatus("12345678901", request, CancellationToken.None);

        // THEN returns 428 Precondition Required
        var statusResult = Assert.IsType<ObjectResult>(result);
        Assert.Equal(428, statusResult.StatusCode);
    }

    [Fact]
    public async Task UpdateLoanStatus_InvalidETag_ReturnsBadRequest()
    {
        // GIVEN an invalid ETag format
        var controller = CreateControllerWithIfMatch("not-base64!!!");
        var request = new UpdateLoanStatusRequest { Status = LoanStatus.Frozen };

        // WHEN updating status
        var result = await controller.UpdateLoanStatus("12345678901", request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task UpdateLoanStatus_NotFound_ReturnsNotFound()
    {
        // GIVEN no loan exists
        var controller = CreateControllerWithIfMatch(Convert.ToBase64String([0x01]));
        var request = new UpdateLoanStatusRequest { Status = LoanStatus.Frozen };

        // WHEN updating status
        var result = await controller.UpdateLoanStatus("99999999999", request, CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    [Fact]
    public async Task UpdateLoanStatus_ConcurrencyConflict_ReturnsConflict()
    {
        // GIVEN a loan with different RowVersion
        var loan = CreateTestLoan("12345678901");
        loan.RowVersion = [0x01, 0x02, 0x03, 0x04];
        await _loanRepo.AddAsync(loan);

        var controller = CreateControllerWithIfMatch(
            Convert.ToBase64String([0xFF, 0xFF, 0xFF, 0xFF]));

        // WHEN updating with stale ETag
        var request = new UpdateLoanStatusRequest { Status = LoanStatus.Frozen };
        var result = await controller.UpdateLoanStatus("12345678901", request, CancellationToken.None);

        // THEN returns Conflict
        Assert.IsType<ConflictObjectResult>(result);
    }

    [Fact]
    public async Task UpdateLoanStatus_InvalidTransition_ReturnsBadRequest()
    {
        // GIVEN a closed loan
        var loan = CreateTestLoan("12345678901");
        loan.ActiveStatus = LoanStatus.Closed;
        loan.RowVersion = [0x01];
        await _loanRepo.AddAsync(loan);

        var controller = CreateControllerWithIfMatch(Convert.ToBase64String([0x01]));

        // WHEN trying invalid transition
        var request = new UpdateLoanStatusRequest { Status = LoanStatus.Active };
        var result = await controller.UpdateLoanStatus("12345678901", request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/loans/{accountId}/schedule — Payment schedule
    // =================================================================

    [Fact]
    public async Task GetPaymentSchedule_ValidLoan_ReturnsOk()
    {
        // GIVEN an active term loan
        var loan = CreateTestLoan("12345678901");
        loan.LoanType = LoanType.TermLoan;
        loan.CurrentBalance = 100000.00m;
        await _loanRepo.AddAsync(loan);

        // WHEN requesting payment schedule
        var controller = CreateController();
        var result = await controller.GetPaymentSchedule(
            "12345678901", 0.06m, 12, CancellationToken.None);

        // THEN returns OK with amortization entries
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<PaymentScheduleResponse>(okResult.Value);
        Assert.Equal(12, response.Schedule.Count);
        Assert.Equal(0m, response.Schedule[^1].RemainingPrincipal);
    }

    [Fact]
    public async Task GetPaymentSchedule_InvalidAccountId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();

        // WHEN requesting schedule
        var result = await controller.GetPaymentSchedule(
            "INVALID", 0.06m, 12, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task GetPaymentSchedule_LoanNotFound_ReturnsNotFound()
    {
        // GIVEN no loan exists
        var controller = CreateController();

        // WHEN requesting schedule
        var result = await controller.GetPaymentSchedule(
            "99999999999", 0.06m, 12, CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    [Fact]
    public async Task GetPaymentSchedule_InvalidTermMonths_ReturnsBadRequest()
    {
        // GIVEN a valid loan
        var loan = CreateTestLoan("12345678901");
        await _loanRepo.AddAsync(loan);

        // WHEN requesting schedule with zero term
        var controller = CreateController();
        var result = await controller.GetPaymentSchedule(
            "12345678901", 0.06m, 0, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/loans/{accountId}/early-repayment — Early repayment calc
    // =================================================================

    [Fact]
    public async Task CalculateEarlyRepayment_ValidLoan_ReturnsOk()
    {
        // GIVEN an active loan with balance
        var loan = CreateTestLoan("12345678901");
        loan.CurrentBalance = 100000.00m;
        await _loanRepo.AddAsync(loan);

        // WHEN calculating early repayment
        var controller = CreateController();
        var result = await controller.CalculateEarlyRepayment(
            "12345678901", 0.06m, 6, CancellationToken.None);

        // THEN returns OK with calculation
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<EarlyRepaymentResponse>(okResult.Value);
        Assert.Equal(100000.00m, response.OutstandingPrincipal);
        Assert.True(response.AccruedInterest >= 0);
        Assert.True(response.TotalRepaymentAmount > 0);
    }

    [Fact]
    public async Task CalculateEarlyRepayment_InvalidAccountId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();

        // WHEN calculating
        var result = await controller.CalculateEarlyRepayment(
            "INVALID", 0.06m, 6, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task CalculateEarlyRepayment_LoanNotFound_ReturnsNotFound()
    {
        // GIVEN no loan exists
        var controller = CreateController();

        // WHEN calculating
        var result = await controller.CalculateEarlyRepayment(
            "99999999999", 0.06m, 6, CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    // =================================================================
    // POST /api/loan-applications — Submit application
    // =================================================================

    [Fact]
    public async Task SubmitApplication_ValidRequest_ReturnsCreated()
    {
        // GIVEN a valid application
        var controller = CreateController();
        var request = new SubmitLoanApplicationRequest
        {
            CustomerId = "CUST00000001",
            ApplicantName = "ERIK LINDSTRÖM",
            RequestedAmount = 500000.00m,
            RequestedTermMonths = 60,
            LoanType = LoanType.Mortgage
        };

        // WHEN submitting
        var result = await controller.SubmitApplication(request, CancellationToken.None);

        // THEN returns Created
        var createdResult = Assert.IsType<CreatedAtActionResult>(result);
        var response = Assert.IsType<LoanApplicationResponse>(createdResult.Value);
        Assert.Equal("CUST00000001", response.CustomerId);
        Assert.Equal("Pending", response.Status);
    }

    [Fact]
    public async Task SubmitApplication_MissingCustomerId_ReturnsBadRequest()
    {
        // GIVEN missing customer ID
        var controller = CreateController();
        var request = new SubmitLoanApplicationRequest
        {
            CustomerId = "",
            ApplicantName = "TEST",
            RequestedAmount = 100000.00m
        };

        // WHEN submitting
        var result = await controller.SubmitApplication(request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task SubmitApplication_InvalidAmount_ReturnsBadRequest()
    {
        // GIVEN zero amount
        var controller = CreateController();
        var request = new SubmitLoanApplicationRequest
        {
            CustomerId = "CUST00000001",
            ApplicantName = "TEST",
            RequestedAmount = 0m
        };

        // WHEN submitting
        var result = await controller.SubmitApplication(request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/loan-applications/{applicationId} — Application detail
    // =================================================================

    [Fact]
    public async Task GetApplication_ValidId_ReturnsOk()
    {
        // GIVEN an application exists
        var application = CreateTestApplication("APP-00000001");
        await _applicationRepo.AddAsync(application);

        // WHEN requesting by ID
        var controller = CreateController();
        var result = await controller.GetApplication("APP-00000001", CancellationToken.None);

        // THEN returns OK
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<LoanApplicationResponse>(okResult.Value);
        Assert.Equal("APP-00000001", response.ApplicationId);
    }

    [Fact]
    public async Task GetApplication_NotFound_ReturnsNotFound()
    {
        // GIVEN no application exists
        var controller = CreateController();

        // WHEN requesting
        var result = await controller.GetApplication("APP-99999999", CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    [Fact]
    public async Task GetApplication_EmptyId_ReturnsBadRequest()
    {
        // GIVEN empty application ID
        var controller = CreateController();

        // WHEN requesting with empty ID
        var result = await controller.GetApplication("", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/loan-applications?customerId= — List applications
    // =================================================================

    [Fact]
    public async Task ListApplications_ValidCustomerId_ReturnsOk()
    {
        // GIVEN applications exist for a customer
        await _applicationRepo.AddAsync(CreateTestApplication("APP-00000001", "CUST00000001"));
        await _applicationRepo.AddAsync(CreateTestApplication("APP-00000002", "CUST00000001"));

        // WHEN listing
        var controller = CreateController();
        var result = await controller.ListApplications("CUST00000001", CancellationToken.None);

        // THEN returns OK
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ListApplications_MissingCustomerId_ReturnsBadRequest()
    {
        // GIVEN no customer ID
        var controller = CreateController();

        // WHEN listing
        var result = await controller.ListApplications("", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // Helpers
    // =================================================================

    private static Loan CreateTestLoan(string accountId) => new()
    {
        AccountId = accountId,
        ActiveStatus = LoanStatus.Active,
        LoanType = LoanType.TermLoan,
        CreditLimit = 500000.00m,
        CashCreditLimit = 50000.00m,
        BorrowerName = "TEST BORROWER",
        DisclosureGroupId = "TERM_STD",
        OriginationDate = new DateTime(2024, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    };

    private static LoanApplication CreateTestApplication(
        string applicationId, string customerId = "CUST00000001") => new()
        {
            ApplicationId = applicationId,
            CustomerId = customerId,
            RequestedAmount = 500000.00m,
            RequestedTermMonths = 60,
            LoanType = LoanType.Mortgage,
            Status = LoanApplicationStatus.Pending,
            ApplicantName = "TEST APPLICANT",
            ApplicationDate = new DateTime(2024, 6, 1, 0, 0, 0, DateTimeKind.Utc)
        };
}

/// <summary>
/// In-memory loan repository for controller unit tests.
/// </summary>
internal sealed class InMemoryLoanRepository : ILoanRepository
{
    private readonly List<Loan> _loans = [];

    public Task<Loan?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default) =>
        Task.FromResult(_loans.FirstOrDefault(l => l.AccountId == accountId));

    public Task<IReadOnlyList<Loan>> GetPageAsync(int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default)
    {
        var query = _loans.AsEnumerable();

        if (!string.IsNullOrEmpty(afterAccountId))
        {
            query = query.Where(l => string.Compare(l.AccountId, afterAccountId, StringComparison.Ordinal) > 0);
        }

        IReadOnlyList<Loan> result = [.. query.OrderBy(l => l.AccountId).Take(pageSize)];
        return Task.FromResult(result);
    }

    public Task AddAsync(Loan loan, CancellationToken cancellationToken = default)
    {
        _loans.Add(loan);
        return Task.CompletedTask;
    }

    public Task UpdateAsync(Loan loan, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;
}

/// <summary>
/// In-memory loan application repository for controller unit tests.
/// </summary>
internal sealed class InMemoryLoanApplicationRepository : ILoanApplicationRepository
{
    private readonly List<LoanApplication> _applications = [];

    public Task<LoanApplication?> GetByIdAsync(string applicationId, CancellationToken cancellationToken = default) =>
        Task.FromResult(_applications.FirstOrDefault(a => a.ApplicationId == applicationId));

    public Task<IReadOnlyList<LoanApplication>> GetByCustomerIdAsync(string customerId, CancellationToken cancellationToken = default)
    {
        IReadOnlyList<LoanApplication> result = [.. _applications.Where(a => a.CustomerId == customerId)];
        return Task.FromResult(result);
    }

    public Task AddAsync(LoanApplication application, CancellationToken cancellationToken = default)
    {
        _applications.Add(application);
        return Task.CompletedTask;
    }

    public Task UpdateAsync(LoanApplication application, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;
}
