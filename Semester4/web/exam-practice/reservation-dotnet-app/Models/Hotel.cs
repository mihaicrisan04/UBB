using System.ComponentModel.DataAnnotations;

namespace ReservationApp.Models;

public class Hotel
{
    [Key]
    public int HotelID { get; set; }
    public string HotelName { get; set; }
    public DateTime Date { get; set; }
    public string City { get; set; }
    public int AvailableRooms { get; set; }
}