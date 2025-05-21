using backend.Data;
using backend.Models;
using Microsoft.AspNetCore.Identity;
using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Http; // Required for StatusCodes
using System.Threading.Tasks; // Required for Task

var MyAllowSpecificOrigins = "_myAllowSpecificOrigins"; // 1. Define policy name

var builder = WebApplication.CreateBuilder(args);

// 2. Add CORS services
builder.Services.AddCors(options =>
{
    options.AddPolicy(name: MyAllowSpecificOrigins,
                      policy  =>
                      {
                          policy.WithOrigins("http://localhost:4200") // Angular default dev port
                                .AllowAnyHeader()
                                .AllowAnyMethod()
                                .AllowCredentials(); // Important for cookie-based auth
                      });
});

// 1. Add Connection String
var connectionString = builder.Configuration.GetConnectionString("DefaultConnection") 
    ?? throw new InvalidOperationException("Connection string 'DefaultConnection' not found.");

// 2. Add DbContext
builder.Services.AddDbContextPool<ApplicationDbContext>(options =>
    options.UseMySql(connectionString, ServerVersion.AutoDetect(connectionString),
        mySqlOptions => 
            mySqlOptions.EnableStringComparisonTranslations()
    )
);

// 3. Add Identity
builder.Services.AddIdentity<ApplicationUser, IdentityRole>(options =>
    {
        options.SignIn.RequireConfirmedAccount = false; // Keep it simple for now
    })
    .AddEntityFrameworkStores<ApplicationDbContext>()
    .AddDefaultTokenProviders();

// Customize Application Cookie behavior for SPA (no redirects, just status codes)
builder.Services.ConfigureApplicationCookie(options =>
{
    options.Events.OnRedirectToLogin = context =>
    {
        context.Response.StatusCode = StatusCodes.Status401Unauthorized;
        return Task.CompletedTask;
    };
    options.Events.OnRedirectToAccessDenied = context =>
    {
        context.Response.StatusCode = StatusCodes.Status403Forbidden;
        return Task.CompletedTask;
    };
    // No need for OnRedirectToLogout if SPA handles logout by calling a backend endpoint that signs out.
});

// Add services to the container.
builder.Services.AddControllers()
    .AddJsonOptions(options => // Configure JSON to use camelCase
    {
        options.JsonSerializerOptions.PropertyNamingPolicy = System.Text.Json.JsonNamingPolicy.CamelCase;
    });

// Learn more about configuring OpenAPI at https://aka.ms/aspnet/openapi
// Keep OpenAPI for API documentation if desired
builder.Services.AddEndpointsApiExplorer(); // Required for minimal APIs if you keep them
builder.Services.AddSwaggerGen(); // For OpenAPI UI

// Configure authentication (e.g., cookie-based)
// You might want to configure cookie options here for session management, timeouts, etc.
// builder.Services.AddAuthentication(IdentityConstants.ApplicationScheme) // Default scheme for Identity
//     .AddIdentityCookies(); // Adds cookie authentication

builder.Services.AddAuthorization(); // Add authorization services

var app = builder.Build();

// Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

// app.UseHttpsRedirection(); // We set --no-https, so this is commented out for now

// 3. Use CORS - Must be before UseRouting, UseAuthentication, UseAuthorization
app.UseCors(MyAllowSpecificOrigins);

app.UseRouting(); // Must come before UseAuthentication and UseAuthorization

app.UseAuthentication(); // Enable authentication middleware
app.UseAuthorization(); // Enable authorization middleware

app.MapControllers(); // Map attribute-routed API controllers

app.Run();
