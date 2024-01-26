


def print_menu() -> None:
    print()
    print("Menu: ")
    print("1. Add a music festival")
    print("2. Show festival playing in season")
    print("3. Show all festivals where artist will perform, sorted by month")
    print("4. Exit")

def print_exception_message(exception_message) -> None:
    print(exception_message)

def print_string_to_console(string: str) -> None:
    print(string)

def read_option() -> int:
    while True:
        try:
            option = int(input("Enter your option: "))
            return option

        except ValueError:
            print_exception_message("You option must be an integer")

def read_festival_name() -> str:
    festival_name = input("Enter the festival name: ")
    return festival_name

def read_month() -> str:
    month = input("Enter the month: ")
    month[0] = month[0].upper()
    return month

def read_price() -> int:
    while True:
        try: 
            price = int(input("Enter the price: "))
            return price

        except ValueError as error:
            print("Incorect price. Try again.")

def read_artists() -> list:
    print("Write the names of the artists separated by comma(,).")
    artists = input("Enter the artists: ")
    artists = artists.split(',')
    artists = [artist.strip(" ") for artist in artists]
    return artists

def read_season() -> str:
    season = input("Enter the season(Summer, Winter, Spring, Autumn): ")
    season = season.strip(" ")
    return season

def read_aritst() -> str:
    artist = input("Enter artist: ")
    return artist
