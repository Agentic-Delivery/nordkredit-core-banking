using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.AccountManagement;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for account management operations.
/// Replaces COBOL account management programs (account lookup, update, lifecycle management).
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD).
/// Business rules: ACCT-BR-001 through ACCT-BR-008.
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15/17, FSA FFFS 2014:5 Ch. 3-4.
/// SEC-BR-001: Internal only — requires OperatorAccess (Azure AD).
/// </summary>
[ApiController]
[Route("api/accounts")]
[Authorize(Policy = "OperatorAccess")]
public class AccountsController : ControllerBase
{
    private readonly IAccountManagementRepository _repository;

    public AccountsController(IAccountManagementRepository repository)
    {
        _repository = repository;
    }

    /// <summary>
    /// Lists accounts with keyset pagination.
    /// Business rule: ACCT-BR-008 (account display/list).
    /// Regulations: PSD2 Art. 97, GDPR Art. 15.
    /// </summary>
    [HttpGet]
    public async Task<IActionResult> ListAccounts(
        [FromQuery] string? afterAccountId,
        CancellationToken cancellationToken)
    {
        if (!string.IsNullOrEmpty(afterAccountId))
        {
            var validation = AccountValidationService.ValidateAccountId(afterAccountId, required: false);
            if (!validation.IsValid)
            {
                return BadRequest(new { Message = validation.ErrorMessage });
            }
        }

        var accounts = await _repository.GetAllAsync(7, afterAccountId, cancellationToken);
        return Ok(accounts.Select(MapToResponse));
    }

    /// <summary>
    /// Retrieves a single account by ID.
    /// COBOL: Account lookup by primary key (ACCT-ID).
    /// Business rules: ACCT-BR-001 (data structure), ACCT-BR-002 (ID validation).
    /// Regulations: PSD2 Art. 97, GDPR Art. 15.
    /// </summary>
    [HttpGet("{accountId}")]
    public async Task<IActionResult> GetAccount(
        string accountId,
        CancellationToken cancellationToken)
    {
        var validation = AccountValidationService.ValidateAccountId(accountId, required: true);
        if (!validation.IsValid)
        {
            return BadRequest(new { Message = validation.ErrorMessage });
        }

        var account = await _repository.GetByIdAsync(accountId, cancellationToken);
        if (account is null)
        {
            return NotFound(new { Message = "Account not found" });
        }

        return Ok(MapToResponse(account));
    }

    /// <summary>
    /// Creates a new account.
    /// Business rules: ACCT-BR-001 (data structure), ACCT-BR-002 (ID validation).
    /// Regulations: FSA FFFS 2014:5 Ch. 3, AML/KYC.
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> CreateAccount(
        [FromBody] CreateAccountRequest request,
        CancellationToken cancellationToken)
    {
        var validation = AccountValidationService.ValidateAccountId(request.AccountId, required: true);
        if (!validation.IsValid)
        {
            return BadRequest(new { Message = validation.ErrorMessage });
        }

        var existing = await _repository.GetByIdAsync(request.AccountId, cancellationToken);
        if (existing is not null)
        {
            return Conflict(new { Message = "Account already exists" });
        }

        var account = new Account
        {
            Id = request.AccountId,
            Status = AccountStatus.Active,
            AccountType = request.AccountType,
            HolderName = request.HolderName,
            CreditLimit = request.CreditLimit,
            CashCreditLimit = request.CashCreditLimit,
            OpenedDate = DateTime.UtcNow
        };

        await _repository.AddAsync(account, cancellationToken);

        return CreatedAtAction(nameof(GetAccount), new { accountId = account.Id }, MapToResponse(account));
    }

    /// <summary>
    /// Updates account status (lifecycle transition).
    /// Business rule: ACCT-BR-005 (status transitions).
    /// State machine: Active → Dormant, Frozen, Closed; Dormant → Active, Frozen, Closed;
    ///                Frozen → Active, Dormant, Closed; Closed → (terminal).
    /// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97, GDPR Art. 17.
    /// </summary>
    [HttpPut("{accountId}/status")]
    public async Task<IActionResult> UpdateAccountStatus(
        string accountId,
        [FromBody] UpdateAccountStatusRequest request,
        CancellationToken cancellationToken)
    {
        var ifMatchValues = Request?.Headers.IfMatch;
        var ifMatch = ifMatchValues?.ToString();
        if (string.IsNullOrEmpty(ifMatch))
        {
            return StatusCode(428, new { Message = "ETag required for update" });
        }

        var idValidation = AccountValidationService.ValidateAccountId(accountId, required: true);
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

        var account = await _repository.GetByIdAsync(accountId, cancellationToken);
        if (account is null)
        {
            return NotFound(new { Message = "Account not found" });
        }

        if (!account.RowVersion.SequenceEqual(rowVersion))
        {
            return Conflict(new { Message = "Account was modified by another user", Account = MapToResponse(account) });
        }

        var transitionResult = account.TransitionTo(request.Status);
        if (!transitionResult.IsValid)
        {
            return BadRequest(new { Message = transitionResult.ErrorMessage });
        }

        await _repository.UpdateAsync(account, cancellationToken);

        return Ok(MapToResponse(account));
    }

    private static AccountResponse MapToResponse(Account account) => new()
    {
        AccountId = account.Id,
        Status = account.Status.ToString(),
        AccountType = account.AccountType.ToString(),
        CurrentBalance = account.CurrentBalance,
        CreditLimit = account.CreditLimit,
        CashCreditLimit = account.CashCreditLimit,
        CurrentCycleCredit = account.CurrentCycleCredit,
        CurrentCycleDebit = account.CurrentCycleDebit,
        ExpirationDate = account.ExpirationDate?.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        HolderName = account.HolderName,
        OpenedDate = account.OpenedDate.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        ClosedDate = account.ClosedDate?.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        ETag = Convert.ToBase64String(account.RowVersion)
    };
}

public class CreateAccountRequest
{
    public string AccountId { get; set; } = string.Empty;
    public AccountType AccountType { get; set; }
    public string HolderName { get; set; } = string.Empty;
    public decimal CreditLimit { get; set; }
    public decimal CashCreditLimit { get; set; }
}

public class UpdateAccountStatusRequest
{
    public AccountStatus Status { get; set; }
}

public class AccountResponse
{
    public string AccountId { get; set; } = string.Empty;
    public string Status { get; set; } = string.Empty;
    public string AccountType { get; set; } = string.Empty;
    public decimal CurrentBalance { get; set; }
    public decimal CreditLimit { get; set; }
    public decimal CashCreditLimit { get; set; }
    public decimal CurrentCycleCredit { get; set; }
    public decimal CurrentCycleDebit { get; set; }
    public string? ExpirationDate { get; set; }
    public string HolderName { get; set; } = string.Empty;
    public string OpenedDate { get; set; } = string.Empty;
    public string? ClosedDate { get; set; }
    public string ETag { get; set; } = string.Empty;
}
