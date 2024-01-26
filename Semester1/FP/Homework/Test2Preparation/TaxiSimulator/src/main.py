from repository import Repository
from service import Service
from ui import UI


def main():
    repo = Repository()
    service = Service(repo)
    ui = UI(service)

    ui.run()


if __name__ == '__main__':
    main()