import ui

def initialize_dummy_data() -> list:
    festivals = [
        ["Untold", 7, 500, ["Armin", "Bon Jovi", "Skillet"]], 
        ["Electric", 5, 400, ["John", "Sky", "Armin"]],
        ["Bumba", 7, 1000, ["Armin", "Skillet"]]
    ]
    return festivals

def add_festival(festivals: list, MONTHS_LIST: list) -> None:
    ui.print_string_to_console("Enter the name, month, price, list of artists")
    name = ui.read_festival_name()
    month = ui.read_month()
    price = ui.read_price()
    artists = ui.read_artists()

    if month not in MONTHS_LIST:
        raise Exception("Incorect Month. Festival not added to the list")
    
    for festival in festivals:
        if festival[0] == name:
            raise Exception("Festival already in the list") 

    festivals.append([name, MONTHS_LIST.index(month) + 1, price, artists])

def show_festival_during_season(festivals: list, MONTHS_LIST: list) -> None:
    SEASONS = ["Summer", "Autumn", "Winter", "Spring"]  
    season = ui.read_season()
    # season = "Summer"

    if season not in SEASONS:
        raise Exception("Wrong season. Try again")

    if season == SEASONS[0]:
        season_months = MONTHS_LIST[5:8]
    elif season == SEASONS[1]:
        season_months = MONTHS_LIST[8:11]
    elif season == SEASONS[2]:
        season_months = MONTHS_LIST[-1] + MONTHS_LIST[0:2]
    else:
        season_months = MONTHS_LIST[2:5]

    desired_festivals = [festival for festival in festivals if MONTHS_LIST[festival[1]-1] in season_months]

    for i in range(0,len(desired_festivals) - 1):
        for j in range(i+1, len(desired_festivals)):
            if desired_festivals[i][1] > desired_festivals[j][1]:
                desired_festivals[i], desired_festivals[j] = desired_festivals[j], desired_festivals[i]
            elif desired_festivals[i][1] == desired_festivals[j][1]:
                if desired_festivals[i][0] > desired_festivals[j][0]:
                    desired_festivals[i], desired_festivals[j] = desired_festivals[j], desired_festivals[i]

    print(desired_festivals)

def show_festival_where_aritst_performs(festivals: list) -> None:
    artist = ui.read_aritst()

    artist_found = False
    for festival in festivals:
        if artist in festival[3]:
            artist_found = True
    
    if artist_found == False:
        raise Exception("There isn't any artist with this name playing at any festival")

    desired_festivals = []
    for festival in festivals:
        if artist in festival[3]:
            desired_festivals.append(festival)

    for i in range(0, len(desired_festivals) - 1):
        for j in range(i + 1, len(desired_festivals)):
            if desired_festivals[i][1] > desired_festivals[j][1]:
                desired_festivals[i][1],  desired_festivals[j][1] = desired_festivals[j][1], desired_festivals[i][1]

    print(desired_festivals)

def run_app() -> None:
    #initializations
    MONTHS_LIST = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    festivals = initialize_dummy_data()

    while True:
        try:
            ui.print_menu()

            option = ui.read_option()

            if option == 1:
                add_festival(festivals, MONTHS_LIST)
            elif option == 2:
                show_festival_during_season(festivals, MONTHS_LIST)
            elif option == 3:
                show_festival_where_aritst_performs(festivals)
            elif option == 4:
                break
            else:
                raise Exception("Invalid option. Option must be between 1 and 4")

            # print(festivals)

        except Exception as e:
            ui.print_exception_message(e)
