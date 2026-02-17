using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;

namespace NordKredit.Infrastructure;

/// <summary>
/// Design-time factory for EF Core migrations tooling.
/// Used by <c>dotnet ef migrations add</c> and <c>dotnet ef migrations script</c>.
/// The connection string is a placeholder — migrations are generated from the model,
/// not by connecting to a live database.
/// </summary>
public class DesignTimeDbContextFactory : IDesignTimeDbContextFactory<NordKreditDbContext>
{
    public NordKreditDbContext CreateDbContext(string[] args)
    {
        var optionsBuilder = new DbContextOptionsBuilder<NordKreditDbContext>();
        optionsBuilder.UseSqlServer("Server=.;Database=NordKredit;Trusted_Connection=True;");

        return new NordKreditDbContext(optionsBuilder.Options);
    }
}
