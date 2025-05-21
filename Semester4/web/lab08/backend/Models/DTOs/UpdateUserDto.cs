using System.ComponentModel.DataAnnotations;

namespace backend.Models.DTOs;

public class UpdateUserDto
{
    // Username and Email are typically not updated directly here or are handled with care due to Identity constraints.
    // Password should have its own dedicated endpoint.

    [StringLength(100)] // Example validation
    public string? Name { get; set; }

    [Range(0, 150)]
    public int? Age { get; set; } // Nullable if update is optional

    public string? Role { get; set; }

    public string? Gender { get; set; }

    [Url] // Validates if it's a URL
    public string? Webpage { get; set; }

    public string? Profile { get; set; }

    // You might add Email if you allow it to be updated,
    // but be aware of Identity's email confirmation flows.
    // public string? Email { get; set; }
} 