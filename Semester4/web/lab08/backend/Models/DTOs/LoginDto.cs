using System.ComponentModel.DataAnnotations;

namespace backend.Models.DTOs;

public class LoginDto
{
    [Required]
    public string? Username { get; set; }

    [Required]
    [DataType(DataType.Password)]
    public string? Password { get; set; }

    // Optional: Remember Me
    public bool RememberMe { get; set; } = false;
} 