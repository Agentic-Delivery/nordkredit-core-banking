using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardsController HTTP response mapping.
/// COBOL source: COCRDLIC.cbl — verifies REST API contract for card list operations.
/// Regulations: PSD2 Art. 97, GDPR Art. 15, FFFS 2014:5 Ch. 8 §4.
/// </summary>
public class CardsControllerTests
{
    private readonly StubCardRepository _repo = new();
    private readonly CardsController _controller;

    public CardsControllerTests()
    {
        var listService = new CardListService(_repo);
        _controller = new CardsController(listService);
    }

    // ===================================================================
    // GET /api/cards — first page (COCRDLIC.cbl:1123-1163)
    // ===================================================================

    [Fact]
    public async Task GetCards_15Cards_NoCursor_Returns200WithFirst7()
    {
        // GIVEN the CARDDAT table contains 15 cards
        _repo.SeedCards(15);

        // WHEN GET /api/cards is called with no filters or cursor
        var result = await _controller.GetCards(
            null, null, null, null, CancellationToken.None);

        // THEN 200 OK with first 7 cards
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(7, body.Cards.Count);
        Assert.True(body.HasNextPage);
        Assert.NotNull(body.FirstCardNumber);
        Assert.NotNull(body.LastCardNumber);
    }

    [Fact]
    public async Task GetCards_WithAfterCursor_ReturnsNextPage()
    {
        // GIVEN a nextCursor from a previous response
        _repo.SeedCards(15);

        // WHEN GET /api/cards?afterCardNumber={cursor} is called
        var result = await _controller.GetCards(
            "4000000000000007", null, null, null, CancellationToken.None);

        // THEN the next 7 cards are returned
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(7, body.Cards.Count);
        Assert.Equal("4000000000000008", body.Cards[0].CardNumber);
    }

    [Fact]
    public async Task GetCards_WithBeforeCursor_ReturnsPreviousPage()
    {
        // GIVEN the user requests backward navigation
        _repo.SeedCards(15);

        // WHEN GET /api/cards?beforeCardNumber={cursor} is called
        var result = await _controller.GetCards(
            null, "4000000000000008", null, null, CancellationToken.None);

        // THEN the previous 7 cards are returned
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(7, body.Cards.Count);
        Assert.Equal("4000000000000001", body.Cards[0].CardNumber);
    }

    [Fact]
    public async Task GetCards_EmptyTable_Returns200WithEmptyArray()
    {
        // GIVEN the CARDDAT table is empty

        // WHEN GET /api/cards is called
        var result = await _controller.GetCards(
            null, null, null, null, CancellationToken.None);

        // THEN 200 OK with empty array and message
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Empty(body.Cards);
        Assert.False(body.HasNextPage);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", body.Message);
    }

    // ===================================================================
    // Account ID filter (COCRDLIC.cbl:1382-1389)
    // ===================================================================

    [Fact]
    public async Task GetCards_AccountIdFilter_ReturnsMatchingCards()
    {
        // GIVEN account filter "12345678901"
        _repo.SeedCardsWithAccounts();

        // WHEN GET /api/cards?accountId=12345678901 is called
        var result = await _controller.GetCards(
            null, null, "12345678901", null, CancellationToken.None);

        // THEN only cards belonging to that account
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.All(body.Cards, c => Assert.Equal("12345678901", c.AccountId));
    }

    [Fact]
    public async Task GetCards_NoMatchingAccount_Returns200WithMessage()
    {
        // GIVEN no records match
        _repo.SeedCards(5);

        // WHEN GET /api/cards?accountId=99999999999 is called
        var result = await _controller.GetCards(
            null, null, "99999999999", null, CancellationToken.None);

        // THEN 200 with empty array and message
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Empty(body.Cards);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", body.Message);
    }

    // ===================================================================
    // Input validation — CARD-BR-004/005 (COCRDLIC.cbl:1003-1034)
    // ===================================================================

    [Fact]
    public async Task GetCards_InvalidAccountId_Returns400BadRequest()
    {
        // GIVEN invalid account filter "ABC"
        // WHEN GET /api/cards?accountId=ABC is called
        var result = await _controller.GetCards(
            null, null, "ABC", null, CancellationToken.None);

        // THEN 400 Bad Request with validation error
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", message);
    }

    [Fact]
    public async Task GetCards_InvalidCardNumber_Returns400BadRequest()
    {
        // GIVEN invalid card number filter
        // WHEN GET /api/cards?cardNumber=XYZ is called
        var result = await _controller.GetCards(
            null, null, null, "XYZ", CancellationToken.None);

        // THEN 400 Bad Request with validation error
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", message);
    }

    [Theory]
    [InlineData("1234567890")]     // Too short
    [InlineData("123456789012")]   // Too long
    [InlineData("00000000000")]    // All zeros
    public async Task GetCards_InvalidAccountFormats_Returns400(string accountId)
    {
        var result = await _controller.GetCards(
            null, null, accountId, null, CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Theory]
    [InlineData("400012345678901")]  // Too short (15 digits)
    [InlineData("40001234567890123")] // Too long (17 digits)
    public async Task GetCards_InvalidCardNumberFormats_Returns400(string cardNumber)
    {
        var result = await _controller.GetCards(
            null, null, null, cardNumber, CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    // ===================================================================
    // Combined filters (COCRDLIC.cbl:1382-1397 — AND logic)
    // ===================================================================

    [Fact]
    public async Task GetCards_BothFilters_ReturnsBothMatching()
    {
        // GIVEN both account and card filters
        _repo.SeedCardsWithAccounts();

        // WHEN GET /api/cards?accountId=12345678901&cardNumber=4000000000000001 is called
        var result = await _controller.GetCards(
            null, null, "12345678901", "4000000000000001", CancellationToken.None);

        // THEN only cards matching BOTH filters
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Single(body.Cards);
        Assert.Equal("4000000000000001", body.Cards[0].CardNumber);
    }

    // ===================================================================
    // CARD-BR-002: Selection routing (COCRDLIC.cbl:77-82, 1073-1115)
    // 'S' maps to GET /api/cards/{cardNumber}
    // 'U' maps to PUT /api/cards/{cardNumber}
    // These are documented via API design — no runtime validation needed
    // because REST API replaces CICS screen action codes with proper HTTP verbs.
    // ===================================================================
}
