using Microsoft.EntityFrameworkCore;
using ReservationApp.Models;

namespace ReservationApp.Data;

public class ApplicationDbContext : DbContext
{
    public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options)
        : base(options) { }

    public DbSet<Flight> Flights { get; set; }
    public DbSet<Hotel> Hotels { get; set; }
    public DbSet<Reservation> Reservations { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        // Map C# property names to database column names if they differ
        modelBuilder.Entity<Flight>().Property(f => f.FlightID).HasColumnName("flightID");
        modelBuilder.Entity<Flight>().Property(f => f.DestinationCity).HasColumnName("destinationCity");
        modelBuilder.Entity<Flight>().Property(f => f.AvailableSeats).HasColumnName("availableSeats");

        modelBuilder.Entity<Hotel>().Property(h => h.HotelID).HasColumnName("hotelID");
        modelBuilder.Entity<Hotel>().Property(h => h.HotelName).HasColumnName("hotelName");
        modelBuilder.Entity<Hotel>().Property(h => h.AvailableRooms).HasColumnName("availableRooms");
    }
}