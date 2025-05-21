using System.ComponentModel.DataAnnotations;

namespace backend.Models.DTOs;

public class RegisterDto
{
    [Required]
    public string? Name { get; set; }

    [Required]
    [EmailAddress]
    public string? Email { get; set; }

    [Required]
    public string? Username { get; set; }

    [Required]
    [DataType(DataType.Password)]
    public string? Password { get; set; }

    [Required]
    [Range(0, 150)] // Assuming a reasonable age range
    public int Age { get; set; }

    [Required]
    public string? Role { get; set; }

    [Required]
    public string? Gender { get; set; }

    public string? Webpage { get; set; } // Optional

    public string? Profile { get; set; } // Optional
} 