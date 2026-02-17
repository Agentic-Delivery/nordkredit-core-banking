using System.Text.Json;

namespace NordKredit.ComparisonTests.Lending;

/// <summary>
/// Comparison tests for loan application processing — parallel-run validation against mainframe output.
/// COBOL source: Dedicated origination program (inferred from CVACT01Y.cpy).
/// Business rule: LND-BR-003 (loan origination and credit assessment).
/// Regulations: FSA FFFS 2014:5 Ch.6, Consumer Credit Directive Art.8 (creditworthiness),
///              AML 2017:11 (customer due diligence), GDPR Art.6(1)(b).
///
/// Golden file: Lending/GoldenFiles/loan-application.json
/// Contains expected mainframe output for loan application processing scenarios.
///
/// Known intentional differences:
/// - Application ID: COBOL uses sequential numeric IDs. Migrated system uses APP-yyyyMMddHHmmssfff format.
/// - Status values: COBOL uses numeric status codes. Migrated system uses Pending/Approved/Rejected/Cancelled.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareLoanApplication_Approved_MatchesMainframeOutput
/// - CompareLoanApplication_Rejected_MatchesMainframeOutput
/// - CompareLoanApplication_AmlKycFailed_MatchesMainframeOutput
/// </summary>
public class LoanApplicationComparisonTests
{
    private const string _goldenFilePath = "Lending/GoldenFiles/loan-application.json";

    [Fact]
    public void GoldenFile_Exists() =>
        Assert.True(File.Exists(_goldenFilePath), $"Golden file not found: {_goldenFilePath}");

    [Fact]
    public void GoldenFile_IsValidJson()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.NotNull(document);
    }

    [Fact]
    public void GoldenFile_ContainsAllApplications()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var applications = document.RootElement.GetProperty("applications");

        Assert.Equal(5, applications.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_ApplicationFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("applications")[0];

        Assert.True(first.TryGetProperty("applicationId", out _));
        Assert.True(first.TryGetProperty("customerId", out _));
        Assert.True(first.TryGetProperty("applicantName", out _));
        Assert.True(first.TryGetProperty("requestedAmount", out _));
        Assert.True(first.TryGetProperty("requestedTermMonths", out _));
        Assert.True(first.TryGetProperty("loanType", out _));
        Assert.True(first.TryGetProperty("status", out _));
        Assert.True(first.TryGetProperty("applicationDate", out _));
        Assert.True(first.TryGetProperty("amlKycPassed", out _));
        Assert.True(first.TryGetProperty("creditAssessmentPassed", out _));
    }

    [Fact]
    public void GoldenFile_ApprovedApplication_HasCreditLimit()
    {
        var application = GetApplicationByStatus("Approved");
        var approvedLimit = application.GetProperty("approvedCreditLimit");

        Assert.NotEqual(JsonValueKind.Null, approvedLimit.ValueKind);
        Assert.True(approvedLimit.GetDecimal() > 0);
    }

    [Fact]
    public void GoldenFile_RejectedApplication_HasRejectionReason()
    {
        var application = GetApplicationByStatus("Rejected");
        var reason = application.GetProperty("rejectionReason");

        Assert.NotEqual(JsonValueKind.Null, reason.ValueKind);
        Assert.False(string.IsNullOrWhiteSpace(reason.GetString()));
    }

    [Fact]
    public void GoldenFile_AmlKycFailed_HasRejectionReason()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var applications = document.RootElement.GetProperty("applications");

        foreach (var app in applications.EnumerateArray())
        {
            if (!app.GetProperty("amlKycPassed").GetBoolean() &&
                app.GetProperty("status").GetString() == "Rejected")
            {
                var reason = app.GetProperty("rejectionReason").GetString();
                Assert.NotNull(reason);
                Assert.Contains("AML", reason);
                return;
            }
        }

        Assert.Fail("Expected an application with AML/KYC failure and rejection reason");
    }

    [Fact]
    public void GoldenFile_PendingApplication_HasNoApprovedLimit()
    {
        var application = GetApplicationByStatus("Pending");
        var approvedLimit = application.GetProperty("approvedCreditLimit");

        Assert.Equal(JsonValueKind.Null, approvedLimit.ValueKind);
    }

    [Fact]
    public void GoldenFile_DateFormat_IsISO8601()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("applications")[0];
        var dateStr = first.GetProperty("applicationDate").GetString();

        Assert.NotNull(dateStr);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", dateStr);
    }

    [Fact]
    public void GoldenFile_Amounts_AreExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var applications = document.RootElement.GetProperty("applications");

        foreach (var app in applications.EnumerateArray())
        {
            var amount = app.GetProperty("requestedAmount").GetDecimal();
            Assert.Equal(amount, decimal.Round(amount, 2));
        }
    }

    private static JsonElement GetApplicationByStatus(string status)
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var applications = document.RootElement.GetProperty("applications");

        foreach (var app in applications.EnumerateArray())
        {
            if (app.GetProperty("status").GetString() == status)
            {
                return app.Clone();
            }
        }

        throw new InvalidOperationException($"Application with status '{status}' not found in golden file");
    }
}
