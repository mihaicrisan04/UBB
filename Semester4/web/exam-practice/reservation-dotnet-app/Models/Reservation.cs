using System.ComponentModel.DataAnnotations;

namespace ReservationApp.Models;

public class Reservation
{
    [Key]
    public int Id { get; set; }
    public string Person { get; set; }
    public DateTime Date { get; set; }
    public string Type { get; set; } // "Flight" or "Hotel"
    public int IdReservedResource { get; set; }
}