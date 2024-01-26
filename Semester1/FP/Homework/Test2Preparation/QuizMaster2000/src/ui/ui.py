from src.services.service_exception import ServiceException


class UI:
    def __init__(self, service):
        self.service = service

    def take_quiz(self, quiz) -> int:
        score = 0
        for question in quiz:
            print()
            print(question.text)
            print('a. ' + question.choice_a)
            print('b. ' + question.choice_b)
            print('c. ' + question.choice_c)

            while True:
                answer = input('Enter your choice(a/b/c): ').strip()
                if answer == 'a':
                    answer = question.choice_a
                    break
                elif answer == 'b':
                    answer = question.choice_b
                    break
                elif answer == 'c':
                    answer = question.choice_c
                    break
            
            if answer == question.correct_choice:
                score += 1 if question.difficulty == 'easy' else 2 if question.difficulty == 'medium' else 3
        
        return score

    def run(self):
        while True:

            command = input('>>>').strip()

            if 'add' in command:
                parameters = command.replace('add', '', 1).strip().split(';')
                try:
                    self.service.add_question(*parameters)
                    print('Question successfully added')
                except ServiceException as se:
                    print(se)
            
            elif 'create' in command:
                parameters = command.replace('create', '', 1).strip().split(' ')
                try:
                    self.service.create_quiz(*parameters)
                    print('Quiz successfully created')
                except ServiceException as se:
                    print(se)

            elif 'start' in command:
                quiz_file = command.replace('start', '', 1).strip()
                quiz = self.service.get_quiz(quiz_file)
                score = self.take_quiz(quiz)
                print(f'You got the score: {score}')

            elif 'exit' in command:
                break

            else:
                print('Invalid command')

        print('Program terminated successfully')