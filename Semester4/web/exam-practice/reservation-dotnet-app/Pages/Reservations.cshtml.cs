using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.EntityFrameworkCore;
using ReservationApp.Data;
using ReservationApp.Models;
using System.Globalization;

namespace ReservationApp.Pages;

public class ReservationsModel : PageModel
{
    private readonly ApplicationDbContext _context;

    public List<Flight> AvailableFlights { get; set; } = new();
    public List<Hotel> AvailableHotels { get; set; } = new();
    public string PersonName { get; set; }
    public DateTime SelectedDate { get; set; }
    public string DestinationCity { get; set; }

    public ReservationsModel(ApplicationDbContext context)
    {
        _context = context;
    }

    public async Task<IActionResult> OnGetAsync()
    {
        if (!LoadSessionData()) return RedirectToPage("/Index");

        AvailableFlights = await _context.Flights
            .Where(f => f.Date == SelectedDate && f.DestinationCity == DestinationCity && f.AvailableSeats > 0)
            .ToListAsync();

        AvailableHotels = await _context.Hotels
            .Where(h => h.Date == SelectedDate && h.City == DestinationCity && h.AvailableRooms > 0)
            .ToListAsync();

        return Page();
    }

    public async Task<IActionResult> OnPostReserveFlightAsync(int id)
    {
        if (!LoadSessionData()) return RedirectToPage("/Index");

        var flight = await _context.Flights.FindAsync(id);
        if (flight != null && flight.AvailableSeats > 0)
        {
            flight.AvailableSeats--;
            var reservation = new Reservation
            {
                Person = PersonName,
                Date = SelectedDate,
                Type = "Flight",
                IdReservedResource = id
            };
            _context.Reservations.Add(reservation);
            await _context.SaveChangesAsync();
        }
        return RedirectToPage();
    }

    public async Task<IActionResult> OnPostReserveHotelAsync(int id)
    {
        if (!LoadSessionData()) return RedirectToPage("/Index");

        var hotel = await _context.Hotels.FindAsync(id);
        if (hotel != null && hotel.AvailableRooms > 0)
        {
            hotel.AvailableRooms--;
            var reservation = new Reservation
            {
                Person = PersonName,
                Date = SelectedDate,
                Type = "Hotel",
                IdReservedResource = id
            };
            _context.Reservations.Add(reservation);
            await _context.SaveChangesAsync();
        }
        return RedirectToPage();
    }

    public async Task<IActionResult> OnPostCancelAllAsync()
    {
        if (!LoadSessionData()) return RedirectToPage("/Index");

        var reservationsToCancel = await _context.Reservations
            .Where(r => r.Person == PersonName && r.Date == SelectedDate)
            .ToListAsync();

        foreach (var reservation in reservationsToCancel)
        {
            if (reservation.Type == "Flight")
            {
                var flight = await _context.Flights.FindAsync(reservation.IdReservedResource);
                if (flight != null) flight.AvailableSeats++;
            }
            else if (reservation.Type == "Hotel")
            {
                var hotel = await _context.Hotels.FindAsync(reservation.IdReservedResource);
                if (hotel != null) hotel.AvailableRooms++;
            }
        }

        _context.Reservations.RemoveRange(reservationsToCancel);
        await _context.SaveChangesAsync();

        return RedirectToPage();
    }

    private bool LoadSessionData()
    {
        PersonName = HttpContext.Session.GetString("PersonName");
        var dateString = HttpContext.Session.GetString("SelectedDate");
        DestinationCity = HttpContext.Session.GetString("DestinationCity");

        if (string.IsNullOrEmpty(PersonName) || string.IsNullOrEmpty(dateString) || string.IsNullOrEmpty(DestinationCity))
        {
            return false;
        }

        SelectedDate = DateTime.ParseExact(dateString, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        return true;
    }
}