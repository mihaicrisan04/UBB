using backend.Models;
using backend.Models.DTOs;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;

namespace backend.Controllers;

[ApiController]
[Route("api/[controller]")]
public class AccountController : ControllerBase
{
    private readonly UserManager<ApplicationUser> _userManager;
    private readonly SignInManager<ApplicationUser> _signInManager;

    public AccountController(UserManager<ApplicationUser> userManager, SignInManager<ApplicationUser> signInManager)
    {
        _userManager = userManager;
        _signInManager = signInManager;
    }

    [HttpPost("register")]
    public async Task<IActionResult> Register([FromBody] RegisterDto registerDto)
    {
        if (!ModelState.IsValid)
        {
            return BadRequest(ModelState);
        }

        var user = new ApplicationUser
        {
            UserName = registerDto.Username, // IdentityUser uses UserName
            Email = registerDto.Email,
            Name = registerDto.Name,
            Age = registerDto.Age,
            Gender = registerDto.Gender,
            Role = registerDto.Role, // You might want to map this to IdentityRoles later
            Webpage = registerDto.Webpage,
            Profile = registerDto.Profile
        };

        var result = await _userManager.CreateAsync(user, registerDto.Password!);

        if (result.Succeeded)
        {
            return Ok(new { Status = "Success", Message = "User registered successfully!", UserId = user.Id });
        }

        foreach (var error in result.Errors)
        {
            ModelState.AddModelError(string.Empty, error.Description);
        }
        return BadRequest(ModelState);
    }

    [HttpPost("login")]
    public async Task<IActionResult> Login([FromBody] LoginDto loginDto)
    {
        if (!ModelState.IsValid)
        {
            return BadRequest(ModelState);
        }

        var result = await _signInManager.PasswordSignInAsync(
            loginDto.Username!,
            loginDto.Password!,
            loginDto.RememberMe,      // isPersistent: if true, cookie persists after browser closed
            lockoutOnFailure: false);  // Whether to lockout user on failed attempts

        if (result.Succeeded)
        {
            var user = await _userManager.FindByNameAsync(loginDto.Username!);
            return Ok(new 
            {
                Status = "Success", 
                Message = "Login successful!", 
                UserId = user?.Id, 
                Username = user?.UserName 
            });
        }

        if (result.IsLockedOut)
        {
            return Unauthorized(new { Status = "Error", Message = "User account locked out." });
        }
        if (result.IsNotAllowed)
        {
             return Unauthorized(new { Status = "Error", Message = "User is not allowed to sign in." });
        }
        return Unauthorized(new { Status = "Error", Message = "Invalid login attempt." });
    }

    [Authorize] 
    [HttpPost("logout")]
    public async Task<IActionResult> Logout()
    {
        await _signInManager.SignOutAsync();
        return Ok(new { Status = "Success", Message = "Logout successful!" });
    }
} 