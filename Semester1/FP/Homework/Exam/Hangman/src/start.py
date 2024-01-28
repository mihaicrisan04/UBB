from service import SentenceService
from repository import SentenceRepository
from ui import UI


def main():
    repository = SentenceRepository("sentences.txt")
    service = SentenceService(repository)
    ui = UI(service)

    ui.run()


if __name__ == '__main__':
    main()