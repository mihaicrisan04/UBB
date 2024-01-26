

def print_menu() -> None:
    print()
    print("Menu:")
    print("1. Add a phone(>>> <manufacturer> <model> <price>)")
    print("2. List phone by manufacturer(>>> <manufacturer>)")
    print("3. Update price of a phone(>>> <manufacturer> <model> <amount>)")
    print("4. Increase price by give percent(>>> <percent> (between -50 and 100))")
    print("5. Exit")

def read_user_input(message: str) -> str:
    return input(message)

def read_option() -> str:
    return input(">>>")

def print_string(string: str) -> None:
    print(string)

def print_exception_message(exception: Exception) -> None:
    print(exception)