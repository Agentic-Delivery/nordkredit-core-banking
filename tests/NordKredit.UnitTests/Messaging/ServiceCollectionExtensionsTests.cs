using Azure.Messaging.ServiceBus;
using Microsoft.Extensions.DependencyInjection;
using NordKredit.Domain.Payments.Messaging;
using NordKredit.Infrastructure.Messaging;

namespace NordKredit.UnitTests.Messaging;

/// <summary>
/// Tests for <see cref="ServiceCollectionExtensions"/>.
/// Verifies DI registration of Azure Service Bus messaging services.
/// </summary>
public class ServiceCollectionExtensionsTests
{
    [Fact]
    public void AddServiceBusMessaging_RegistersServiceBusClient()
    {
        var services = new ServiceCollection();
        services.AddLogging();

        services.AddServiceBusMessaging("Endpoint=sb://fake.servicebus.windows.net/;SharedAccessKeyName=Test;SharedAccessKey=dGVzdA==");

        var provider = services.BuildServiceProvider();
        var client = provider.GetService<ServiceBusClient>();
        Assert.NotNull(client);
    }

    [Fact]
    public void AddServiceBusMessaging_RegistersMessagePublisher()
    {
        var services = new ServiceCollection();
        services.AddLogging();

        services.AddServiceBusMessaging("Endpoint=sb://fake.servicebus.windows.net/;SharedAccessKeyName=Test;SharedAccessKey=dGVzdA==");

        var provider = services.BuildServiceProvider();
        var publisher = provider.GetService<IMessagePublisher>();
        Assert.NotNull(publisher);
        Assert.IsType<ServiceBusMessagePublisher>(publisher);
    }

    [Fact]
    public void AddServiceBusMessaging_RegistersMessageConsumer()
    {
        var services = new ServiceCollection();
        services.AddLogging();

        services.AddServiceBusMessaging("Endpoint=sb://fake.servicebus.windows.net/;SharedAccessKeyName=Test;SharedAccessKey=dGVzdA==");

        var provider = services.BuildServiceProvider();
        var consumer = provider.GetService<IMessageConsumer>();
        Assert.NotNull(consumer);
        Assert.IsType<ServiceBusMessageConsumer>(consumer);
    }

    [Fact]
    public void AddServiceBusMessaging_NullConnectionString_ThrowsArgumentException()
    {
        var services = new ServiceCollection();

        Assert.Throws<ArgumentException>(() =>
            services.AddServiceBusMessaging(""));
    }

    [Fact]
    public void AddServiceBusMessaging_WhitespaceConnectionString_ThrowsArgumentException()
    {
        var services = new ServiceCollection();

        Assert.Throws<ArgumentException>(() =>
            services.AddServiceBusMessaging("   "));
    }

    [Fact]
    public void AddServiceBusMessaging_ReturnsSameServiceCollection()
    {
        var services = new ServiceCollection();
        services.AddLogging();

        var result = services.AddServiceBusMessaging("Endpoint=sb://fake.servicebus.windows.net/;SharedAccessKeyName=Test;SharedAccessKey=dGVzdA==");

        Assert.Same(services, result);
    }
}
