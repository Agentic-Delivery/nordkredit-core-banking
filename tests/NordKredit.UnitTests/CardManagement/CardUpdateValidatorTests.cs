using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardUpdateValidator.
/// COBOL source: COCRDUPC.cbl:806-947, 87-99, 255-263.
/// Covers card update field validation (CARD-BR-006).
/// Regulations: FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97, GDPR Art. 5(1)(d).
/// </summary>
public class CardUpdateValidatorTests
{
    // ===================================================================
    // Embossed Name Validation — CARD-BR-006, paragraph 1230
    // COBOL: COCRDUPC.cbl:806-843
    // ===================================================================

    [Fact]
    public void ValidateEmbossedName_ValidAlphaAndSpaces_Passes()
    {
        var result = CardUpdateValidator.ValidateEmbossedName("JANE DOE");

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Fact]
    public void ValidateEmbossedName_WithNumbers_ReturnsAlphaError()
    {
        var result = CardUpdateValidator.ValidateEmbossedName("JOHN DOE 3RD");

        Assert.False(result.IsValid);
        Assert.Equal("Card name can only contain alphabets and spaces", result.ErrorMessage);
    }

    [Fact]
    public void ValidateEmbossedName_SwedishCharacters_Passes()
    {
        // Edge case #1: extend COBOL A-Z to include Swedish characters (Å, Ä, Ö)
        var result = CardUpdateValidator.ValidateEmbossedName("BJÖRK ÅSTRÖM");

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Fact]
    public void ValidateEmbossedName_Blank_ReturnsNotProvided()
    {
        var result = CardUpdateValidator.ValidateEmbossedName("");

        Assert.False(result.IsValid);
        Assert.Equal("Card name not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateEmbossedName_Null_ReturnsNotProvided()
    {
        var result = CardUpdateValidator.ValidateEmbossedName(null);

        Assert.False(result.IsValid);
        Assert.Equal("Card name not provided", result.ErrorMessage);
    }

    [Fact]
    public void ValidateEmbossedName_WhitespaceOnly_ReturnsNotProvided()
    {
        var result = CardUpdateValidator.ValidateEmbossedName("   ");

        Assert.False(result.IsValid);
        Assert.Equal("Card name not provided", result.ErrorMessage);
    }

    [Theory]
    [InlineData("JOHN-DOE")]
    [InlineData("O'BRIEN")]
    [InlineData("NAME@EMAIL")]
    [InlineData("JOHN123")]
    public void ValidateEmbossedName_SpecialCharacters_ReturnsAlphaError(string input)
    {
        var result = CardUpdateValidator.ValidateEmbossedName(input);

        Assert.False(result.IsValid);
        Assert.Equal("Card name can only contain alphabets and spaces", result.ErrorMessage);
    }

    [Fact]
    public void ValidateEmbossedName_LowercaseAlpha_Passes()
    {
        var result = CardUpdateValidator.ValidateEmbossedName("jane doe");

        Assert.True(result.IsValid);
    }

    [Fact]
    public void ValidateEmbossedName_MixedCaseSwedish_Passes()
    {
        var result = CardUpdateValidator.ValidateEmbossedName("Björk Åström Öberg Ärling");

        Assert.True(result.IsValid);
    }

    // ===================================================================
    // Active Status Validation — CARD-BR-006, paragraph 1240
    // COBOL: COCRDUPC.cbl:845-876
    // ===================================================================

    [Theory]
    [InlineData('Y')]
    [InlineData('N')]
    public void ValidateActiveStatus_ValidValues_Passes(char status)
    {
        var result = CardUpdateValidator.ValidateActiveStatus(status);

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Theory]
    [InlineData('A')]
    [InlineData('X')]
    [InlineData('y')]
    [InlineData('n')]
    [InlineData(' ')]
    [InlineData('0')]
    public void ValidateActiveStatus_InvalidValues_ReturnsError(char status)
    {
        var result = CardUpdateValidator.ValidateActiveStatus(status);

        Assert.False(result.IsValid);
        Assert.Equal("Card Active Status must be Y or N", result.ErrorMessage);
    }

    // ===================================================================
    // Expiry Month Validation — CARD-BR-006, paragraph 1250
    // COBOL: COCRDUPC.cbl:877-912
    // ===================================================================

    [Theory]
    [InlineData(1)]
    [InlineData(6)]
    [InlineData(12)]
    public void ValidateExpiryMonth_ValidRange_Passes(int month)
    {
        var result = CardUpdateValidator.ValidateExpiryMonth(month);

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(13)]
    [InlineData(-1)]
    [InlineData(99)]
    public void ValidateExpiryMonth_OutOfRange_ReturnsError(int month)
    {
        var result = CardUpdateValidator.ValidateExpiryMonth(month);

        Assert.False(result.IsValid);
        Assert.Equal("Card expiry month must be between 1 and 12", result.ErrorMessage);
    }

    // ===================================================================
    // Expiry Year Validation — CARD-BR-006, paragraph 1260
    // COBOL: COCRDUPC.cbl:913-947
    // ===================================================================

    [Theory]
    [InlineData(1950)]
    [InlineData(2028)]
    [InlineData(2099)]
    public void ValidateExpiryYear_ValidRange_Passes(int year)
    {
        var result = CardUpdateValidator.ValidateExpiryYear(year);

        Assert.True(result.IsValid);
        Assert.Null(result.ErrorMessage);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1949)]
    [InlineData(2100)]
    [InlineData(9999)]
    public void ValidateExpiryYear_OutOfRange_ReturnsError(int year)
    {
        var result = CardUpdateValidator.ValidateExpiryYear(year);

        Assert.False(result.IsValid);
        Assert.Equal("Invalid card expiry year", result.ErrorMessage);
    }

    // ===================================================================
    // Change Detection — CARD-BR-006, paragraph 1200
    // COBOL: COCRDUPC.cbl case-insensitive comparison via UPPER-CASE
    // ===================================================================

    [Fact]
    public void DetectChanges_NoChanges_ReturnsNoChangeMessage()
    {
        var existing = new Card
        {
            EmbossedName = "JOHN DOE",
            ActiveStatus = 'Y',
            ExpirationDate = new DateOnly(2028, 6, 15)
        };

        var result = CardUpdateValidator.DetectChanges(
            existing,
            newEmbossedName: "JOHN DOE",
            newActiveStatus: 'Y',
            newExpiryMonth: 6,
            newExpiryYear: 2028);

        Assert.False(result.HasChanges);
        Assert.Equal("No change detected with respect to values fetched.", result.Message);
    }

    [Fact]
    public void DetectChanges_CaseInsensitive_ReturnsNoChange()
    {
        // Acceptance criteria scenario 12: case-insensitive comparison
        var existing = new Card
        {
            EmbossedName = "JOHN DOE",
            ActiveStatus = 'Y',
            ExpirationDate = new DateOnly(2028, 6, 15)
        };

        var result = CardUpdateValidator.DetectChanges(
            existing,
            newEmbossedName: "John Doe",
            newActiveStatus: 'Y',
            newExpiryMonth: 6,
            newExpiryYear: 2028);

        Assert.False(result.HasChanges);
        Assert.Equal("No change detected with respect to values fetched.", result.Message);
    }

    [Fact]
    public void DetectChanges_NameChanged_ReturnsHasChanges()
    {
        var existing = new Card
        {
            EmbossedName = "JOHN DOE",
            ActiveStatus = 'Y',
            ExpirationDate = new DateOnly(2028, 6, 15)
        };

        var result = CardUpdateValidator.DetectChanges(
            existing,
            newEmbossedName: "JANE DOE",
            newActiveStatus: 'Y',
            newExpiryMonth: 6,
            newExpiryYear: 2028);

        Assert.True(result.HasChanges);
        Assert.Null(result.Message);
    }

    [Fact]
    public void DetectChanges_StatusChanged_ReturnsHasChanges()
    {
        var existing = new Card
        {
            EmbossedName = "JOHN DOE",
            ActiveStatus = 'Y',
            ExpirationDate = new DateOnly(2028, 6, 15)
        };

        var result = CardUpdateValidator.DetectChanges(
            existing,
            newEmbossedName: "JOHN DOE",
            newActiveStatus: 'N',
            newExpiryMonth: 6,
            newExpiryYear: 2028);

        Assert.True(result.HasChanges);
    }

    [Fact]
    public void DetectChanges_ExpiryMonthChanged_ReturnsHasChanges()
    {
        var existing = new Card
        {
            EmbossedName = "JOHN DOE",
            ActiveStatus = 'Y',
            ExpirationDate = new DateOnly(2028, 6, 15)
        };

        var result = CardUpdateValidator.DetectChanges(
            existing,
            newEmbossedName: "JOHN DOE",
            newActiveStatus: 'Y',
            newExpiryMonth: 7,
            newExpiryYear: 2028);

        Assert.True(result.HasChanges);
    }

    [Fact]
    public void DetectChanges_ExpiryYearChanged_ReturnsHasChanges()
    {
        var existing = new Card
        {
            EmbossedName = "JOHN DOE",
            ActiveStatus = 'Y',
            ExpirationDate = new DateOnly(2028, 6, 15)
        };

        var result = CardUpdateValidator.DetectChanges(
            existing,
            newEmbossedName: "JOHN DOE",
            newActiveStatus: 'Y',
            newExpiryMonth: 6,
            newExpiryYear: 2029);

        Assert.True(result.HasChanges);
    }

    // ===================================================================
    // Full Update Validation — CARD-BR-006
    // Improvement: returns ALL errors simultaneously (COBOL only returned first)
    // ===================================================================

    [Fact]
    public void ValidateUpdate_AllFieldsValid_Passes()
    {
        var result = CardUpdateValidator.ValidateUpdate(
            embossedName: "JANE DOE",
            activeStatus: 'Y',
            expiryMonth: 6,
            expiryYear: 2028);

        Assert.True(result.IsValid);
        Assert.Empty(result.Errors);
    }

    [Fact]
    public void ValidateUpdate_MultipleInvalidFields_ReturnsAllErrors()
    {
        // Acceptance criteria: ALL errors returned simultaneously
        var result = CardUpdateValidator.ValidateUpdate(
            embossedName: "",
            activeStatus: 'A',
            expiryMonth: 13,
            expiryYear: 2100);

        Assert.False(result.IsValid);
        Assert.Equal(4, result.Errors.Count);
        Assert.Contains("Card name not provided", result.Errors);
        Assert.Contains("Card Active Status must be Y or N", result.Errors);
        Assert.Contains("Card expiry month must be between 1 and 12", result.Errors);
        Assert.Contains("Invalid card expiry year", result.Errors);
    }

    [Fact]
    public void ValidateUpdate_SingleInvalidField_ReturnsSingleError()
    {
        var result = CardUpdateValidator.ValidateUpdate(
            embossedName: "JOHN DOE 3RD",
            activeStatus: 'Y',
            expiryMonth: 6,
            expiryYear: 2028);

        Assert.False(result.IsValid);
        Assert.Single(result.Errors);
        Assert.Contains("Card name can only contain alphabets and spaces", result.Errors);
    }
}
