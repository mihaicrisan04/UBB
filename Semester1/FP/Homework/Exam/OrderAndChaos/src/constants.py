SIZE = 6
START_NEW_GAME_OPTION = '1'
LOAD_GAME_OPTION = '2'
BACK_OPTION = '0'
PAUSE_OPTION = '0'
RESUME_OPTION = '0'
EXIT_OPTION = '0'
SAVE_AND_EXIT_OPTION = '9'

BOARD_OPTIONS = []
for i in range(SIZE):
    for j in range(SIZE):
        BOARD_OPTIONS.append(chr(i + 65) + str(j + 1))
        BOARD_OPTIONS.append(chr(i + 97) + str(j + 1))
        BOARD_OPTIONS.append(str(i + 1) + chr(j + 65))
        BOARD_OPTIONS.append(str(i + 1) + chr(j + 97))
BOARD_OPTIONS.append(PAUSE_OPTION)
