using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.Deposits;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for deposit account operations.
/// Replaces COBOL CICS online transaction screens for deposit management, account inquiry, and interest inquiry.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), CBTRN02C.cbl (transaction processing).
/// Business rules: DEP-BR-001 through DEP-BR-009.
/// Regulations: FSA FFFS 2014:5 Ch. 3 (deposit operations), Deposit Guarantee Directive 2014/49/EU,
///              PSD2 Art. 64, GDPR Art. 5/15, AML 2017:11.
/// SEC-BR-001: Internal only — requires OperatorAccess (Azure AD).
/// </summary>
[ApiController]
[Route("api/deposits")]
[Authorize(Policy = "OperatorAccess")]
public class DepositsController : ControllerBase
{
    private readonly IDepositAccountRepository _accountRepository;
    private readonly ISavingsProductRepository _productRepository;

    public DepositsController(
        IDepositAccountRepository accountRepository,
        ISavingsProductRepository productRepository)
    {
        _accountRepository = accountRepository;
        _productRepository = productRepository;
    }

    /// <summary>
    /// Lists deposit accounts with keyset pagination.
    /// COBOL: Sequential read with STARTBR/READNEXT pattern on ACCTFILE.
    /// Business rule: DEP-BR-001 (data structure).
    /// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 15.
    /// </summary>
    [HttpGet]
    public async Task<IActionResult> ListDeposits(
        [FromQuery] string? afterAccountId,
        CancellationToken cancellationToken)
    {
        if (!string.IsNullOrEmpty(afterAccountId))
        {
            var validation = DepositValidationService.ValidateAccountId(afterAccountId);
            if (!validation.IsValid)
            {
                return BadRequest(new { Message = validation.ErrorMessage });
            }
        }

        var accounts = await _accountRepository.GetPageAsync(7, afterAccountId, cancellationToken);
        return Ok(accounts.Select(MapToResponse));
    }

    /// <summary>
    /// Retrieves a single deposit account by account ID (status inquiry).
    /// COBOL: READ ACCOUNT-FILE using FD-ACCT-ID.
    /// Business rules: DEP-BR-001 (data structure), DEP-BR-003 (balance).
    /// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive, GDPR Art. 15.
    /// </summary>
    [HttpGet("{accountId}")]
    public async Task<IActionResult> GetDeposit(
        string accountId,
        CancellationToken cancellationToken)
    {
        var validation = DepositValidationService.ValidateAccountId(accountId);
        if (!validation.IsValid)
        {
            return BadRequest(new { Message = validation.ErrorMessage });
        }

        var account = await _accountRepository.GetByIdAsync(accountId, cancellationToken);
        if (account is null)
        {
            return NotFound(new { Message = "Deposit account not found" });
        }

        return Ok(MapToResponse(account));
    }

    /// <summary>
    /// Creates a new deposit account.
    /// COBOL: WRITE ACCOUNT-RECORD to ACCTFILE.
    /// Business rules: DEP-BR-001 (data structure), DEP-BR-002 (account opening).
    /// Regulations: FSA FFFS 2014:5 Ch. 3, AML 2017:11, Deposit Guarantee Directive.
    /// </summary>
    [HttpPost]
    public async Task<IActionResult> CreateDeposit(
        [FromBody] CreateDepositRequest request,
        CancellationToken cancellationToken)
    {
        var idValidation = DepositValidationService.ValidateAccountId(request.AccountId);
        if (!idValidation.IsValid)
        {
            return BadRequest(new { Message = idValidation.ErrorMessage });
        }

        var groupValidation = DepositValidationService.ValidateDisclosureGroupId(request.DisclosureGroupId);
        if (!groupValidation.IsValid)
        {
            return BadRequest(new { Message = groupValidation.ErrorMessage });
        }

        var existing = await _accountRepository.GetByIdAsync(request.AccountId, cancellationToken);
        if (existing is not null)
        {
            return Conflict(new { Message = "Deposit account already exists" });
        }

        var account = new DepositAccount
        {
            Id = request.AccountId,
            Status = DepositAccountStatus.Active,
            ProductType = request.ProductType,
            HolderName = request.HolderName,
            DisclosureGroupId = request.DisclosureGroupId,
            OpenedDate = DateTime.UtcNow,
            MaturityDate = request.MaturityDate
        };

        await _accountRepository.AddAsync(account, cancellationToken);

        return CreatedAtAction(nameof(GetDeposit), new { accountId = account.Id }, MapToResponse(account));
    }

    /// <summary>
    /// Updates deposit account status (lifecycle transition).
    /// Business rule: DEP-BR-009 (dormancy management, status state machine).
    /// State machine: Active → Dormant, Frozen, Closed; Dormant → Active, Frozen, Closed; etc.
    /// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 17 (right to erasure on closure).
    /// </summary>
    [HttpPut("{accountId}/status")]
    public async Task<IActionResult> UpdateDepositStatus(
        string accountId,
        [FromBody] UpdateDepositStatusRequest request,
        CancellationToken cancellationToken)
    {
        var ifMatchValues = Request?.Headers.IfMatch;
        var ifMatch = ifMatchValues?.ToString();
        if (string.IsNullOrEmpty(ifMatch))
        {
            return StatusCode(428, new { Message = "ETag required for update" });
        }

        var idValidation = DepositValidationService.ValidateAccountId(accountId);
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

        var account = await _accountRepository.GetByIdAsync(accountId, cancellationToken);
        if (account is null)
        {
            return NotFound(new { Message = "Deposit account not found" });
        }

        if (!account.RowVersion.SequenceEqual(rowVersion))
        {
            return Conflict(new { Message = "Deposit account was modified by another user", Account = MapToResponse(account) });
        }

        var transitionResult = account.TransitionTo(request.Status);
        if (!transitionResult.IsValid)
        {
            return BadRequest(new { Message = transitionResult.ErrorMessage });
        }

        await _accountRepository.UpdateAsync(account, cancellationToken);

        return Ok(MapToResponse(account));
    }

    /// <summary>
    /// Retrieves interest inquiry for a deposit account.
    /// COBOL source: Dedicated interest calculation batch (not yet in repository).
    /// Business rule: DEP-BR-004 (interest calculation and accrual).
    /// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
    /// </summary>
    [HttpGet("{accountId}/interest")]
    public async Task<IActionResult> GetInterestInquiry(
        string accountId,
        CancellationToken cancellationToken)
    {
        var idValidation = DepositValidationService.ValidateAccountId(accountId);
        if (!idValidation.IsValid)
        {
            return BadRequest(new { Message = idValidation.ErrorMessage });
        }

        var account = await _accountRepository.GetByIdAsync(accountId, cancellationToken);
        if (account is null)
        {
            return NotFound(new { Message = "Deposit account not found" });
        }

        var product = await _productRepository.GetByProductIdAsync(account.DisclosureGroupId, cancellationToken);
        if (product is null)
        {
            return NotFound(new { Message = "Savings product not found for this account" });
        }

        var dailyInterest = InterestCalculation.CalculateDailyInterest(account.CurrentBalance, product);

        return Ok(new InterestInquiryResponse
        {
            AccountId = account.Id,
            CurrentBalance = account.CurrentBalance,
            AccruedInterest = account.AccruedInterest,
            AnnualRate = product.AnnualRate,
            DailyInterestAmount = dailyInterest,
            LastInterestPostingDate = account.LastInterestPostingDate?.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
            ProductId = product.ProductId,
            ProductDescription = product.Description
        });
    }

    private static DepositAccountResponse MapToResponse(DepositAccount account) => new()
    {
        AccountId = account.Id,
        Status = account.Status.ToString(),
        ProductType = account.ProductType.ToString(),
        CurrentBalance = account.CurrentBalance,
        CurrentCycleCredit = account.CurrentCycleCredit,
        CurrentCycleDebit = account.CurrentCycleDebit,
        DisclosureGroupId = account.DisclosureGroupId,
        MaturityDate = account.MaturityDate?.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        HolderName = account.HolderName,
        OpenedDate = account.OpenedDate.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        ClosedDate = account.ClosedDate?.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        AccruedInterest = account.AccruedInterest,
        ETag = Convert.ToBase64String(account.RowVersion)
    };
}

public class CreateDepositRequest
{
    public string AccountId { get; set; } = string.Empty;
    public DepositProductType ProductType { get; set; }
    public string HolderName { get; set; } = string.Empty;
    public string DisclosureGroupId { get; set; } = string.Empty;
    public DateTime? MaturityDate { get; set; }
}

public class UpdateDepositStatusRequest
{
    public DepositAccountStatus Status { get; set; }
}

public class DepositAccountResponse
{
    public string AccountId { get; set; } = string.Empty;
    public string Status { get; set; } = string.Empty;
    public string ProductType { get; set; } = string.Empty;
    public decimal CurrentBalance { get; set; }
    public decimal CurrentCycleCredit { get; set; }
    public decimal CurrentCycleDebit { get; set; }
    public string DisclosureGroupId { get; set; } = string.Empty;
    public string? MaturityDate { get; set; }
    public string HolderName { get; set; } = string.Empty;
    public string OpenedDate { get; set; } = string.Empty;
    public string? ClosedDate { get; set; }
    public decimal AccruedInterest { get; set; }
    public string ETag { get; set; } = string.Empty;
}

public class InterestInquiryResponse
{
    public string AccountId { get; set; } = string.Empty;
    public decimal CurrentBalance { get; set; }
    public decimal AccruedInterest { get; set; }
    public decimal AnnualRate { get; set; }
    public decimal DailyInterestAmount { get; set; }
    public string? LastInterestPostingDate { get; set; }
    public string ProductId { get; set; } = string.Empty;
    public string ProductDescription { get; set; } = string.Empty;
}
