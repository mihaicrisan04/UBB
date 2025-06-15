using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;

namespace ReservationApp.Pages;

public class IndexModel : PageModel
{
    [BindProperty]
    public string PersonName { get; set; }

    [BindProperty]
    public DateTime SelectedDate { get; set; } = DateTime.Today;

    [BindProperty]
    public string DestinationCity { get; set; }

    public void OnGet() { }

    public IActionResult OnPost()
    {
        if (!ModelState.IsValid)
        {
            return Page();
        }

        HttpContext.Session.SetString("PersonName", PersonName);
        HttpContext.Session.SetString("SelectedDate", SelectedDate.ToString("yyyy-MM-dd"));
        HttpContext.Session.SetString("DestinationCity", DestinationCity);

        return RedirectToPage("/Reservations");
    }
}