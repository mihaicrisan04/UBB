using System.ComponentModel.DataAnnotations;

namespace ReservationApp.Models;

public class Flight
{
    [Key]
    public int FlightID { get; set; }
    public DateTime Date { get; set; }
    public string DestinationCity { get; set; }
    public int AvailableSeats { get; set; }
}