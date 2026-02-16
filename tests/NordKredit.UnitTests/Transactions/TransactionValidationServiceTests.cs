using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionValidationService.
/// COBOL source: COTRN02C.cbl:164-437.
/// Covers key field validation, mandatory field checks, data type validation,
/// amount format validation, date validation, and confirmation flow.
/// Regulations: PSD2 Art.97, FFFS 2014:5 Ch.4 §3, AML/KYC.
/// </summary>
public class TransactionValidationServiceTests
{
    private readonly StubCardCrossReferenceRepository _crossRefRepo = new();
    private readonly TransactionValidationService _sut;

    public TransactionValidationServiceTests()
    {
        _sut = new TransactionValidationService(_crossRefRepo);
    }

    private static TransactionAddRequest CreateValidRequest() => new()
    {
        AccountId = "00000000001",
        CardNumber = "",
        TypeCode = "01",
        CategoryCode = "1001",
        Source = "ONLINE",
        Description = "Test transaction",
        Amount = "+00000100.00",
        OriginationDate = "2026-01-15",
        ProcessingDate = "2026-01-16",
        MerchantId = "000000001",
        MerchantName = "Test Merchant",
        MerchantCity = "Stockholm",
        MerchantZip = "11122",
        Confirm = "Y"
    };

    // ===================================================================
    // Key Field Validation — Decision Table (COTRN02C.cbl:193-230)
    // ===================================================================

    [Fact]
    public async Task AccountId_Exists_PopulatesCardNumber()
    {
        _crossRefRepo.AddByAccountId("00000000001", new CardCrossReference
        {
            AccountId = "00000000001",
            CardNumber = "4000123456789010",
            CustomerId = 100000001
        });

        var request = CreateValidRequest();
        var result = await _sut.ValidateAsync(request);

        Assert.True(result.IsValid);
        Assert.Equal("4000123456789010", result.ResolvedCardNumber);
        Assert.Equal("00000000001", result.ResolvedAccountId);
    }

    [Fact]
    public async Task AccountId_NotFound_ReturnsError()
    {
        // No cross-reference entry for this account
        var request = CreateValidRequest();
        request.AccountId = "99999999999";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Account ID NOT found", result.ErrorMessage);
    }

    [Fact]
    public async Task AccountId_NonNumeric_ReturnsError()
    {
        var request = CreateValidRequest();
        request.AccountId = "ABC";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Account ID must be Numeric", result.ErrorMessage);
    }

    [Fact]
    public async Task CardNumber_Exists_PopulatesAccountId()
    {
        _crossRefRepo.AddByCardNumber("4000123456789010", new CardCrossReference
        {
            CardNumber = "4000123456789010",
            AccountId = "00000000001",
            CustomerId = 100000001
        });

        var request = CreateValidRequest();
        request.AccountId = "";
        request.CardNumber = "4000123456789010";

        var result = await _sut.ValidateAsync(request);

        Assert.True(result.IsValid);
        Assert.Equal("4000123456789010", result.ResolvedCardNumber);
        Assert.Equal("00000000001", result.ResolvedAccountId);
    }

    [Fact]
    public async Task CardNumber_NotFound_ReturnsError()
    {
        var request = CreateValidRequest();
        request.AccountId = "";
        request.CardNumber = "1234567890123456";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Card Number NOT found", result.ErrorMessage);
    }

    [Fact]
    public async Task CardNumber_NonNumeric_ReturnsError()
    {
        var request = CreateValidRequest();
        request.AccountId = "";
        request.CardNumber = "ABCD1234EFGH5678";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Card Number must be Numeric", result.ErrorMessage);
    }

    [Fact]
    public async Task BothKeyFieldsEmpty_ReturnsError()
    {
        var request = CreateValidRequest();
        request.AccountId = "";
        request.CardNumber = "";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Account or Card Number must be entered", result.ErrorMessage);
    }

    // ===================================================================
    // Mandatory Field Validation — 11 fields (COTRN02C.cbl:235-337)
    // ===================================================================

    [Theory]
    [InlineData("TypeCode", "Type CD can NOT be empty")]
    [InlineData("CategoryCode", "Category CD can NOT be empty")]
    [InlineData("Source", "Source can NOT be empty")]
    [InlineData("Description", "Description can NOT be empty")]
    [InlineData("Amount", "Amount can NOT be empty")]
    [InlineData("OriginationDate", "Orig Date can NOT be empty")]
    [InlineData("ProcessingDate", "Proc Date can NOT be empty")]
    [InlineData("MerchantId", "Merchant ID can NOT be empty")]
    [InlineData("MerchantName", "Merchant Name can NOT be empty")]
    [InlineData("MerchantCity", "Merchant City can NOT be empty")]
    [InlineData("MerchantZip", "Merchant Zip can NOT be empty")]
    public async Task MandatoryField_Empty_ReturnsError(string fieldName, string expectedError)
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        SetField(request, fieldName, "");

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains(expectedError, result.ErrorMessage);
    }

    [Theory]
    [InlineData("TypeCode", "   ", "Type CD can NOT be empty")]
    [InlineData("CategoryCode", "   ", "Category CD can NOT be empty")]
    public async Task MandatoryField_Spaces_ReturnsError(string fieldName, string value, string expectedError)
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        SetField(request, fieldName, value);

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains(expectedError, result.ErrorMessage);
    }

    // ===================================================================
    // Data Type Validation — numeric checks (COTRN02C.cbl:283-337)
    // ===================================================================

    [Fact]
    public async Task TypeCode_NonNumeric_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.TypeCode = "AB";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Type CD must be Numeric", result.ErrorMessage);
    }

    [Fact]
    public async Task CategoryCode_NonNumeric_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.CategoryCode = "ABCD";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Category CD must be Numeric", result.ErrorMessage);
    }

    [Fact]
    public async Task MerchantId_NonNumeric_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.MerchantId = "ABC123DEF";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Merchant ID must be Numeric", result.ErrorMessage);
    }

    // ===================================================================
    // Amount Format Validation (COTRN02C.cbl:339-345)
    // ===================================================================

    [Theory]
    [InlineData("12345", "Amount should be in format -99999999.99")]
    [InlineData("ABC", "Amount should be in format -99999999.99")]
    [InlineData("+ABCDEFGH.12", "Amount should be in format -99999999.99")]
    [InlineData("+12345678X12", "Amount should be in format -99999999.99")]
    [InlineData("+12345678.1A", "Amount should be in format -99999999.99")]
    public async Task Amount_InvalidFormat_ReturnsError(string amount, string expectedError)
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Amount = amount;

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains(expectedError, result.ErrorMessage);
    }

    [Theory]
    [InlineData("+00000100.00")]
    [InlineData("-00000100.00")]
    [InlineData("+99999999.99")]
    [InlineData("-00000000.01")]
    public async Task Amount_ValidFormat_PassesValidation(string amount)
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Amount = amount;

        var result = await _sut.ValidateAsync(request);

        Assert.True(result.IsValid);
    }

    // ===================================================================
    // Date Validation (COTRN02C.cbl:347-427)
    // ===================================================================

    [Fact]
    public async Task OriginationDate_InvalidFormat_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.OriginationDate = "15-01-2026";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Orig Date", result.ErrorMessage);
    }

    [Fact]
    public async Task OriginationDate_InvalidCalendarDate_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.OriginationDate = "2026-02-30";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Orig Date", result.ErrorMessage);
        Assert.Contains("Not a valid date", result.ErrorMessage);
    }

    [Fact]
    public async Task ProcessingDate_InvalidCalendarDate_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.ProcessingDate = "2026-13-01";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Proc Date", result.ErrorMessage);
    }

    [Fact]
    public async Task ProcessingDate_InvalidFormat_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.ProcessingDate = "01/16/2026";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Proc Date", result.ErrorMessage);
    }

    // ===================================================================
    // Confirmation Flow (COTRN02C.cbl:429-437)
    // ===================================================================

    [Fact]
    public async Task Confirm_Y_PassesValidation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "Y";

        var result = await _sut.ValidateAsync(request);

        Assert.True(result.IsValid);
        Assert.False(result.ConfirmationRequired);
    }

    [Fact]
    public async Task Confirm_LowercaseY_PassesValidation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "y";

        var result = await _sut.ValidateAsync(request);

        Assert.True(result.IsValid);
        Assert.False(result.ConfirmationRequired);
    }

    [Fact]
    public async Task Confirm_Empty_RequiresConfirmation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.True(result.ConfirmationRequired);
    }

    [Fact]
    public async Task Confirm_N_RequiresConfirmation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "N";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.True(result.ConfirmationRequired);
    }

    [Fact]
    public async Task Confirm_LowercaseN_RequiresConfirmation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "n";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.True(result.ConfirmationRequired);
    }

    [Fact]
    public async Task Confirm_InvalidValue_ReturnsError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "X";

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.False(result.ConfirmationRequired);
        Assert.Contains("Invalid value", result.ErrorMessage);
    }

    // ===================================================================
    // Sequential Validation — exits on first error (COBOL behavior)
    // ===================================================================

    [Fact]
    public async Task Validation_ExitsOnFirstError()
    {
        // Both key fields empty AND all data fields empty — should only get the key field error
        var request = new TransactionAddRequest();

        var result = await _sut.ValidateAsync(request);

        Assert.False(result.IsValid);
        Assert.Contains("Account or Card Number must be entered", result.ErrorMessage);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private void SetupValidCrossReference()
    {
        _crossRefRepo.AddByAccountId("00000000001", new CardCrossReference
        {
            AccountId = "00000000001",
            CardNumber = "4000123456789010",
            CustomerId = 100000001
        });
    }

    private static void SetField(TransactionAddRequest request, string fieldName, string value)
    {
        var property = typeof(TransactionAddRequest).GetProperty(fieldName)
            ?? throw new ArgumentException($"Unknown field: {fieldName}");
        property.SetValue(request, value);
    }
}

/// <summary>
/// In-memory test double for ICardCrossReferenceRepository.
/// Supports both lookup directions for cross-reference validation testing.
/// </summary>
internal sealed class StubCardCrossReferenceRepository : ICardCrossReferenceRepository
{
    private readonly Dictionary<string, CardCrossReference> _byCardNumber = [];
    private readonly Dictionary<string, CardCrossReference> _byAccountId = [];

    public void AddByCardNumber(string cardNumber, CardCrossReference xref)
        => _byCardNumber[cardNumber] = xref;

    public void AddByAccountId(string accountId, CardCrossReference xref)
        => _byAccountId[accountId] = xref;

    public Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default)
        => Task.FromResult(_byCardNumber.GetValueOrDefault(cardNumber));

    public Task<CardCrossReference?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_byAccountId.GetValueOrDefault(accountId));
}
