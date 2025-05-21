using backend.Models;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using Bogus; // For generating fake data
using Microsoft.AspNetCore.Authorization; // To potentially protect the endpoint
using System.Linq; // For Any()

namespace backend.Controllers;

[ApiController]
[Route("api/[controller]")]
public class SetupController : ControllerBase
{
    private readonly UserManager<ApplicationUser> _userManager;
    private readonly RoleManager<IdentityRole> _roleManager; // If you want to assign roles

    public SetupController(UserManager<ApplicationUser> userManager, RoleManager<IdentityRole> roleManager)
    {
        _userManager = userManager;
        _roleManager = roleManager;
    }

    // POST: api/setup/seed-users?count=10
    // [Authorize(Roles = "Admin")] // Recommended to protect this endpoint
    // Or use: #if DEBUG ... #endif to only compile this in debug mode
    [HttpPost("seed-users")]
    public async Task<IActionResult> SeedUsers([FromQuery] int count = 10)
    {
        if (count <= 0)
        {
            return BadRequest(new { Status = "Error", Message = "Count must be a positive integer." });
        }

        // Optional: Check if users already exist to prevent re-seeding unintentionally
        if (_userManager.Users.Any())
        {
            // return Ok(new { Status = "Info", Message = "Database already contains users. Seeding aborted." });
            // Or, allow adding more users if desired. For now, let's allow adding more.
        }

        // Ensure some basic roles exist if you plan to assign them
        string[] roleNames = { "Admin", "User", "Editor" };
        foreach (var roleName in roleNames)
        {
            var roleExist = await _roleManager.RoleExistsAsync(roleName);
            if (!roleExist)
            {
                await _roleManager.CreateAsync(new IdentityRole(roleName));
            }
        }
        
        var faker = new Faker<ApplicationUser>()
            .RuleFor(u => u.UserName, f => f.Internet.UserName(f.Name.FirstName(), f.Name.LastName()))
            .RuleFor(u => u.Email, (f, u) => f.Internet.Email(u.UserName))
            .RuleFor(u => u.Name, f => f.Name.FullName())
            .RuleFor(u => u.Age, f => f.Random.Number(18, 70))
            .RuleFor(u => u.Gender, f => f.PickRandom("Male", "Female", "Other"))
            .RuleFor(u => u.Webpage, f => f.Internet.Url().OrDefault(f, .1f)) // 10% chance of having a webpage
            .RuleFor(u => u.Profile, f => f.Lorem.Sentence().OrDefault(f, .3f)) // 30% chance of having a profile
            .RuleFor(u => u.EmailConfirmed, true)
            .RuleFor(u => u.Role, f => f.PickRandom(roleNames)); // Assign a random role

        int usersCreatedCount = 0;
        for (int i = 0; i < count; i++)
        {
            var newUser = faker.Generate();
            // Passwords should be strong enough for Identity's default requirements
            var result = await _userManager.CreateAsync(newUser, "P@$$wOrd123!"); 

            if (result.Succeeded)
            {
                usersCreatedCount++;
                // Assign the user to their specified role
                // Note: The ApplicationUser.Role property is just a string. 
                // For Identity's role system, you'd use UserManager.AddToRoleAsync
                if (!string.IsNullOrEmpty(newUser.Role))
                {
                    await _userManager.AddToRoleAsync(newUser, newUser.Role);
                }
            }
            else
            {
                // Log errors or collect them
                Console.WriteLine($"Error creating user {newUser.UserName}: {string.Join(", ", result.Errors.Select(e => e.Description))}");
            }
        }

        if (usersCreatedCount > 0)
        {
            return Ok(new { Status = "Success", Message = $"{usersCreatedCount} users seeded successfully." });
        }
        else
        {
            return BadRequest(new { Status = "Error", Message = "No users were created. Check logs for details." });
        }
    }
} 