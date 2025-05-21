namespace backend.Models.DTOs;

public class UserDto
{
    public string? Id { get; set; }
    public string? Name { get; set; }
    public string? Username { get; set; }
    public string? Email { get; set; }
    public int Age { get; set; }
    public string? Role { get; set; }
    public string? Gender { get; set; }
    public string? Webpage { get; set; }
    public string? Profile { get; set; }
    // Add any other fields you want to expose, e.g., EmailConfirmed, LockoutEnd
} 