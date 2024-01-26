from repository import ReservationRepository, RoomRepository
from service import ReservationService, RoomService
from ui import UI


def main():
    room_repository = RoomRepository('rooms.txt')
    reservation_repository = ReservationRepository('reservations.txt')  
    room_service = RoomService(room_repository)
    reservation_service = ReservationService(reservation_repository)
    ui = UI(room_service, reservation_service)

    ui.run()


if __name__ == '__main__':
    main()