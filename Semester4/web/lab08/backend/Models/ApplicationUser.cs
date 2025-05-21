using Microsoft.AspNetCore.Identity;

namespace backend.Models;

public class ApplicationUser : IdentityUser
{
    public string? Name { get; set; } 
    public int Age { get; set; }
    public string? Role { get; set; } 
    public string? Gender { get; set; }
    public string? Webpage { get; set; }
    public string? Profile { get; set; }
} 