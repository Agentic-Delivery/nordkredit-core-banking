using System.Text.Json;

namespace NordKredit.ComparisonTests.Lending;

/// <summary>
/// Comparison tests for lending nightly batch pipeline — parallel-run validation against mainframe output.
/// COBOL source: Nightly lending batch JCL — 3-step pipeline: amortization processing,
///               collateral valuation, delinquency monitoring.
/// Business rules: LND-BR-004 (interest), LND-BR-006 (collateral), LND-BR-008 (delinquency).
/// Regulations: FSA FFFS 2014:5 Ch.4 §3 (operational risk), Ch.6 (credit risk),
///              DORA Art.11 (ICT risk management), AML 2017:11.
///
/// Golden file: Lending/GoldenFiles/lending-batch-output.json
/// Contains expected mainframe output for the lending nightly batch pipeline.
///
/// Known intentional differences:
/// - Batch format: COBOL generates fixed-width report file with control totals.
///   Migrated system generates structured JSON. Content is semantically identical.
/// - SLA monitoring: COBOL relies on JCL job scheduler timestamps.
///   Migrated system includes SLA breach flag in output.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareLendingBatch_AmortizationTotals_MatchesMainframeOutput
/// - CompareLendingBatch_CollateralValuation_MatchesMainframeOutput
/// - CompareLendingBatch_DelinquencyMonitoring_MatchesMainframeOutput
/// </summary>
public class LendingBatchComparisonTests
{
    private const string _goldenFilePath = "Lending/GoldenFiles/lending-batch-output.json";

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
    public void GoldenFile_ContainsBatchResultFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("success", out _));
        Assert.True(root.TryGetProperty("slaBreached", out _));
        Assert.True(root.TryGetProperty("amortizationResult", out _));
        Assert.True(root.TryGetProperty("collateralValuationResult", out _));
        Assert.True(root.TryGetProperty("delinquencyMonitoringResult", out _));
    }

    [Fact]
    public void GoldenFile_AmortizationResult_ProcessedCountIsConsistent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var result = document.RootElement.GetProperty("amortizationResult");

        var total = result.GetProperty("totalProcessed").GetInt32();
        var success = result.GetProperty("successCount").GetInt32();
        var skipped = result.GetProperty("skippedCount").GetInt32();
        var failed = result.GetProperty("failedCount").GetInt32();

        Assert.Equal(total, success + skipped + failed);
    }

    [Fact]
    public void GoldenFile_AmortizationResult_LoanCountMatchesArray()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var result = document.RootElement.GetProperty("amortizationResult");

        var total = result.GetProperty("totalProcessed").GetInt32();
        var loans = result.GetProperty("processedLoans").GetArrayLength();

        Assert.Equal(total, loans);
    }

    [Fact]
    public void GoldenFile_AmortizationResult_TotalInterestMatchesSumOfLoans()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var result = document.RootElement.GetProperty("amortizationResult");

        var totalInterest = result.GetProperty("totalInterestAccrued").GetDecimal();
        var loans = result.GetProperty("processedLoans");

        decimal sumOfInterest = 0;
        foreach (var loan in loans.EnumerateArray())
        {
            if (loan.GetProperty("isSuccess").GetBoolean())
            {
                sumOfInterest += loan.GetProperty("interestAmount").GetDecimal();
            }
        }

        Assert.Equal(totalInterest, sumOfInterest);
    }

    [Fact]
    public void GoldenFile_AmortizationResult_FailedLoansHaveReason()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var loans = document.RootElement
            .GetProperty("amortizationResult")
            .GetProperty("processedLoans");

        foreach (var loan in loans.EnumerateArray())
        {
            if (!loan.GetProperty("isSuccess").GetBoolean())
            {
                var reason = loan.GetProperty("failureReason");
                Assert.NotEqual(JsonValueKind.Null, reason.ValueKind);
                Assert.False(string.IsNullOrWhiteSpace(reason.GetString()));
            }
        }
    }

    [Fact]
    public void GoldenFile_CollateralValuation_ProcessedCountIsConsistent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var result = document.RootElement.GetProperty("collateralValuationResult");

        var total = result.GetProperty("totalProcessed").GetInt32();
        var within = result.GetProperty("withinLtvLimitCount").GetInt32();
        var exceeding = result.GetProperty("exceedingLtvLimitCount").GetInt32();

        Assert.Equal(total, within + exceeding);
    }

    [Fact]
    public void GoldenFile_CollateralValuation_LoanCountMatchesArray()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var result = document.RootElement.GetProperty("collateralValuationResult");

        var total = result.GetProperty("totalProcessed").GetInt32();
        var loans = result.GetProperty("valuatedLoans").GetArrayLength();

        Assert.Equal(total, loans);
    }

    [Fact]
    public void GoldenFile_CollateralValuation_LtvRatioIsConsistent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var loans = document.RootElement
            .GetProperty("collateralValuationResult")
            .GetProperty("valuatedLoans");

        foreach (var loan in loans.EnumerateArray())
        {
            var balance = loan.GetProperty("currentBalance").GetDecimal();
            var collateralValue = loan.GetProperty("totalCollateralValue").GetDecimal();
            var ltvRatio = loan.GetProperty("ltvRatio").GetDecimal();

            if (collateralValue > 0)
            {
                var expectedLtv = decimal.Round(balance / collateralValue * 100m, 2);
                Assert.Equal(expectedLtv, ltvRatio);
            }
        }
    }

    [Fact]
    public void GoldenFile_DelinquencyMonitoring_FieldsArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var result = document.RootElement.GetProperty("delinquencyMonitoringResult");

        Assert.True(result.TryGetProperty("totalProcessed", out _));
        Assert.True(result.TryGetProperty("newlyDelinquentCount", out _));
        Assert.True(result.TryGetProperty("flaggedForAmlScreeningCount", out _));
        Assert.True(result.TryGetProperty("elevatedRiskCount", out _));
    }

    [Fact]
    public void GoldenFile_Amounts_AreExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var loans = document.RootElement
            .GetProperty("amortizationResult")
            .GetProperty("processedLoans");

        foreach (var loan in loans.EnumerateArray())
        {
            var interest = loan.GetProperty("interestAmount").GetDecimal();
            Assert.Equal(interest, decimal.Round(interest, 2));

            var balance = loan.GetProperty("currentBalance").GetDecimal();
            Assert.Equal(balance, decimal.Round(balance, 2));
        }
    }
}
