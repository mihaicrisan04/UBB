import ui


def initializa_dummy_data() -> dict:
    phones_inventroy = {
        "Samsung": [["Note 9", 3000], ["Galaxy S5", 2000], ["Galaxy 7", 3000], ["Galaxy S10", 3500], ["Galaxy S23", 4000]],
        "Apple": [["Iphone 8", 2000], ["Iphone 10", 2500], ["Iphone 13", 3700], ["Iphone 12", 3000]],
        "Nothing": [["Nothin Phone 2", 3000]]
    }
    return phones_inventroy

def translate_option(option: str) -> int:
    if option.isnumeric():
        if int(option) < 1 or int(option) > 5:
            raise Exception("Invalid option. Option must be between 1 and 5")
        else:
            return int(option)
    else:
        raise Exception("Invalid command. Try again")

def add_phone(phones_inventory) -> None:
    """
    Function to add a phoen to the phone inventory 
    Raises exception if the input is wrong or if the phone alredy exists or if the price is not a number

    Parameters:
    :phones_inverntory (dict): phones inventory
    """
    ui.print_string("Enter the manufacturer, the model, and the price: ")

    manufacturer = ui.read_user_input("Enter the manufacutrer: ").strip(" ")
    model = ui.read_user_input("Enter the model: ").strip(" ")
    price = ui.read_user_input("Enter the price: ").strip(" ")

    if len(manufacturer) < 3 or len(model) < 3 or len(price) < 3:
        raise Exception("Incorect user input. Add phone failed. Try again")
    else:

        if price.isnumeric() == False:
            raise Exception("Incorect Price. Add phone failed. Try again")
        
        price = int(price)

        if manufacturer in phones_inventory:
            for phone in phones_inventory[manufacturer]:
                if phone[0] == model:
                    raise Exception("Phone already exists. Add phone failed. Try again")
            
        phones_inventory[manufacturer].append([model, price])

def list_phones_by_manufacturer(phones_inventory: dict) -> None:
    """
    Function that lists all the phone by a manufacturer
    Raises an error if the manufacturer doesn't exist

    Parameters
    :phones_inventory (dict): phones inventory
    """
    input = ui.read_user_input("Enter the manufacturer: ")
    manufacturer = input.strip(" ")

    if manufacturer not in phones_inventory:
        raise Exception("The manufacturer does not exist in the current inventory")
    else:
        ui.print_string(manufacturer + " phones: ")
        for phone in phones_inventory[manufacturer]:
            ui.print_string(phone[0] + ", price: " + str(phone[1]) + " lei")

def increase_price_by_amount(phones_inventory: dict) -> None:
    """
    Function that increases the price of a phone having a manufacturer and a model by a given amount
    Raises Exception if one of the input manufacturer, model or amound is wrong

    Parameters
    :phones_inventory (dict): phones inventory
    """
    ui.print_string("Enter the manufacturer, the model, and the amount: ")

    manufacturer = ui.read_user_input("Enter the manufacutrer: ").strip(" ")
    model = ui.read_user_input("Enter the model: ").strip(" ")
    amount = ui.read_user_input("Enter the amount: ").strip(" ")

    if amount.isnumeric() == False:
        raise Exception("Incorect Amount. Increase price of a phone with a given amount failed. Try again")
        
    amount = int(amount)

    if manufacturer in phones_inventory:
        phone_model_in_inventory_flag = False
        phone_position = -1
        for phone in phones_inventory[manufacturer]:
            if phone[0] == model:
                phone[1] += amount
                phone_model_in_inventory_flag = True

        if phone_model_in_inventory_flag == False:
            raise Exception("Phone Model doesn't exist. Increase price of a phone with a given amount failed. Try again")
    else:
        raise Exception("Manufacturer doesn't exist. Increase price of a phone with a given amount failed. Try again")

def increase_price_by_percent(phones_inventory: dict) -> None:
    """
    Function that updates the price of every phone by a given percent
    The percentage must be between -50 and 100
    Raises Exception if the percent doesn't satisfy the given condition

    Parameters
    :phones_inventory (dict): phones inventory
    """
    percent = ui.read_user_input("Enter the percent: ")

    sign = 1
    if percent[0] == '-':
        sign = -1
        percent = percent[1:]
    
    if percent.isnumeric() == False:
        raise Exception("Percent must be a number. Increase price by percent failed. Try again")
        
    percent = int(percent)
    percent *= sign

    for manufacturer, models in phones_inventory.items():
        for phone in models:
            phone[1] += round(percent * phone[1] / 100)
    
def run_app() -> None:
    """
    Function that contains the main loop of the program.
    It keeps promtin the user for a choice untill the usert decides to stop the program and enter 5 as an exit choice
    """
    # initializations
    phones_inventory = initializa_dummy_data()


    while True:
        try:
            ui.print_menu()

            option = ui.read_option()
            option = translate_option(option)

            if option == 1:
                add_phone(phones_inventory)
            elif option == 2:
                list_phones_by_manufacturer(phones_inventory)
            elif option == 3:
                increase_price_by_amount(phones_inventory)
            elif option == 4:
                increase_price_by_percent(phones_inventory)
            else:
                break

        except Exception as e:
            ui.print_exception_message(e)