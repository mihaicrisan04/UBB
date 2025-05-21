using backend.Models;
using backend.Models.DTOs;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore; // Required for ToListAsync() and other EF Core operations

namespace backend.Controllers;

[Authorize] // Protect all actions in this controller by default
[ApiController]
[Route("api/[controller]")]
public class UsersController : ControllerBase
{
    private readonly UserManager<ApplicationUser> _userManager;

    public UsersController(UserManager<ApplicationUser> userManager)
    {
        _userManager = userManager;
    }

    // GET: api/users
    [HttpGet]
    public async Task<ActionResult<IEnumerable<UserDto>>> GetUsers([FromQuery] string? role)
    {
        IQueryable<ApplicationUser> query = _userManager.Users;

        if (!string.IsNullOrWhiteSpace(role))
        {
            // Assuming 'role' is a simple string property on ApplicationUser that we want to filter by.
            // For Identity's built-in role system, you'd use UserManager.GetUsersInRoleAsync(role)
            // or query through navigation properties if ApplicationUser is linked to IdentityRole.
            // Our ApplicationUser.Role is a string property, so direct string comparison works.
            query = query.Where(u => u.Role != null && u.Role.Equals(role, StringComparison.OrdinalIgnoreCase));
        }

        var users = await query.ToListAsync(); 

        // Map ApplicationUser to UserDto
        var userDtos = users.Select(user => new UserDto
        {
            Id = user.Id,
            Name = user.Name,
            Username = user.UserName, // Note: UserName from IdentityUser
            Email = user.Email,
            Age = user.Age,
            Role = user.Role,
            Gender = user.Gender,
            Webpage = user.Webpage,
            Profile = user.Profile
            // Map other properties as needed
        }).ToList();

        return Ok(userDtos);
    }

    // GET: api/users/{id}
    [HttpGet("{id}")]
    public async Task<ActionResult<UserDto>> GetUser(string id)
    {
        var user = await _userManager.FindByIdAsync(id);

        if (user == null)
        {
            return NotFound(new { Status = "Error", Message = "User not found." });
        }

        // Map ApplicationUser to UserDto
        var userDto = new UserDto
        {
            Id = user.Id,
            Name = user.Name,
            Username = user.UserName,
            Email = user.Email,
            Age = user.Age,
            Role = user.Role,
            Gender = user.Gender,
            Webpage = user.Webpage,
            Profile = user.Profile
            // Map other properties as needed
        };

        return Ok(userDto);
    }

    // PUT: api/users/{id}
    [HttpPut("{id}")]
    public async Task<IActionResult> UpdateUser(string id, [FromBody] UpdateUserDto updateUserDto)
    {
        if (!ModelState.IsValid)
        {
            return BadRequest(ModelState);
        }

        var user = await _userManager.FindByIdAsync(id);
        if (user == null)
        {
            return NotFound(new { Status = "Error", Message = "User not found." });
        }

        // Optional: Check if the current user is the one being updated or an admin.
        // For now, allowing a user to update their own information.
        // var currentUserId = User.FindFirstValue(ClaimTypes.NameIdentifier);
        // if (currentUserId != id /* && !User.IsInRole("Admin") */) // Admin role check needs to be set up
        // {
        //     return Forbid();
        // }

        // Update only the fields that are provided in the DTO
        if (updateUserDto.Name != null) user.Name = updateUserDto.Name;
        if (updateUserDto.Age.HasValue) user.Age = updateUserDto.Age.Value;
        if (updateUserDto.Role != null) user.Role = updateUserDto.Role; // Again, consider IdentityRoles
        if (updateUserDto.Gender != null) user.Gender = updateUserDto.Gender;
        if (updateUserDto.Webpage != null) user.Webpage = updateUserDto.Webpage;
        if (updateUserDto.Profile != null) user.Profile = updateUserDto.Profile;
        // if (updateUserDto.Email != null) { /* Handle email update carefully */ }

        var result = await _userManager.UpdateAsync(user);
        if (result.Succeeded)
        {
            // Consider returning the updated user DTO or just NoContent
            return Ok(new { Status = "Success", Message = "User updated successfully." });
        }

        foreach (var error in result.Errors)
        {
            ModelState.AddModelError(string.Empty, error.Description);
        }
        return BadRequest(ModelState);
    }

    // DELETE: api/users/{id}
    [HttpDelete("{id}")]
    // [Authorize(Roles = "Admin")] // TODO: Add role-based authorization later
    public async Task<IActionResult> DeleteUser(string id)
    {
        var user = await _userManager.FindByIdAsync(id);
        if (user == null)
        {
            return NotFound(new { Status = "Error", Message = "User not found." });
        }

        // TODO: Add security check: Is the current user an Admin or the user themselves?
        // var currentUserId = User.FindFirstValue(ClaimTypes.NameIdentifier);
        // if (currentUserId != id && !User.IsInRole("Admin"))
        // {
        //     return Forbid(); 
        // }

        var result = await _userManager.DeleteAsync(user);
        if (result.Succeeded)
        {
            return Ok(new { Status = "Success", Message = "User deleted successfully." });
            // Consider returning NoContent() if no response body is needed.
        }

        foreach (var error in result.Errors)
        {
            ModelState.AddModelError(string.Empty, error.Description);
        }
        return BadRequest(ModelState); // Or a more specific error response like InternalServerError
    }

    // GET: api/users/search?query=...
    [HttpGet("search")]
    public async Task<ActionResult<IEnumerable<UserDto>>> SearchUsers([FromQuery] string? query)
    {
        if (string.IsNullOrWhiteSpace(query))
        {
            // Return an empty list with OK status if query is empty or missing
            return Ok(Enumerable.Empty<UserDto>());
        }

        // Case-insensitive search on UserName, Email, and Name
        // You can expand this to other fields as needed.
        var lowerQuery = query.ToLower();
        var users = await _userManager.Users
            .Where(u => 
                (u.UserName != null && u.UserName.ToLower().Contains(lowerQuery)) ||
                (u.Email != null && u.Email.ToLower().Contains(lowerQuery)) ||
                (u.Name != null && u.Name.ToLower().Contains(lowerQuery)) ||
                (u.Role != null && u.Role.ToLower().Contains(lowerQuery))
            )
            .ToListAsync();

        if (!users.Any())
        {
            return Ok(Enumerable.Empty<UserDto>()); // Return an empty list with OK status if no users are found
        }

        var userDtos = users.Select(user => new UserDto
        {
            Id = user.Id,
            Name = user.Name,
            Username = user.UserName,
            Email = user.Email,
            Age = user.Age,
            Role = user.Role,
            Gender = user.Gender,
            Webpage = user.Webpage,
            Profile = user.Profile
        }).ToList();

        return Ok(userDtos);
    }

    // We will add other CRUD endpoints here:
} 