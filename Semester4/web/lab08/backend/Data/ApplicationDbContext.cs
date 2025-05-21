using backend.Models;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore;

namespace backend.Data;

public class ApplicationDbContext : IdentityDbContext<ApplicationUser>
{
    public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options)
        : base(options)
    {
    }

    // If you had other DbSet properties for other entities, they would go here.
    // For example: public DbSet<YourOtherEntity> YourOtherEntities { get; set; }

    protected override void OnModelCreating(ModelBuilder builder)
    {
        base.OnModelCreating(builder);
        // Customize the ASP.NET Identity model and override the defaults if needed.
        // For example, you can rename the ASP.NET Identity table names here
        // builder.Entity<ApplicationUser>(entity => { entity.ToTable(name: "Users"); });
        // builder.Entity<IdentityRole>(entity => { entity.ToTable(name: "Roles"); });
        // builder.Entity<IdentityUserRole<string>>(entity => { entity.ToTable("UserRoles"); });
        // builder.Entity<IdentityUserClaim<string>>(entity => { entity.ToTable("UserClaims"); });
        // builder.Entity<IdentityUserLogin<string>>(entity => { entity.ToTable("UserLogins"); });
        // builder.Entity<IdentityRoleClaim<string>>(entity => { entity.ToTable("RoleClaims"); });
        // builder.Entity<IdentityUserToken<string>>(entity => { entity.ToTable("UserTokens"); });
    }
} 