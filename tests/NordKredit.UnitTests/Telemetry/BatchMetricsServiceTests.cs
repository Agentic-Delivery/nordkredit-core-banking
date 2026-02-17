using System.Diagnostics.Metrics;
using NordKredit.Functions.Telemetry;

namespace NordKredit.UnitTests.Telemetry;

public class BatchMetricsServiceTests : IDisposable
{
    private readonly IMeterFactory _meterFactory;
    private readonly BatchMetricsService _sut;

    public BatchMetricsServiceTests()
    {
        _meterFactory = new TestMeterFactory();
        _sut = new BatchMetricsService(_meterFactory);
    }

    [Fact]
    public void BatchDuration_histogram_is_created() => Assert.NotNull(_sut.BatchDuration);

    [Fact]
    public void TransactionsPosted_counter_is_created() => Assert.NotNull(_sut.TransactionsPosted);

    [Fact]
    public void TransactionsRejected_counter_is_created() => Assert.NotNull(_sut.TransactionsRejected);

    [Fact]
    public void BatchDuration_can_record_value()
    {
        var exception = Record.Exception(() => _sut.BatchDuration.Record(42.5));
        Assert.Null(exception);
    }

    [Fact]
    public void TransactionsPosted_can_add_value()
    {
        var exception = Record.Exception(() => _sut.TransactionsPosted.Add(100));
        Assert.Null(exception);
    }

    [Fact]
    public void TransactionsRejected_can_add_value()
    {
        var exception = Record.Exception(() => _sut.TransactionsRejected.Add(5));
        Assert.Null(exception);
    }

    [Fact]
    public void Dispose_does_not_throw()
    {
        var exception = Record.Exception(_sut.Dispose);
        Assert.Null(exception);
    }

    public void Dispose()
    {
        _sut.Dispose();
        GC.SuppressFinalize(this);
    }

    private sealed class TestMeterFactory : IMeterFactory
    {
        private readonly List<Meter> _meters = [];

        public Meter Create(MeterOptions options)
        {
            var meter = new Meter(options.Name, options.Version);
            _meters.Add(meter);
            return meter;
        }

        public void Dispose()
        {
            foreach (var meter in _meters)
            {
                meter.Dispose();
            }
        }
    }
}
