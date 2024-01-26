from repository import Repository
from service import Service
from ui import UI


def main():
    repository = Repository('players.txt')
    service = Service(repository)
    ui = UI(service)

    ui.run()


if __name__ == '__main__':
    main()