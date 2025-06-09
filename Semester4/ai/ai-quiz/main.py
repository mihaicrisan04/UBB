import json
import random
import time
from typing import List, Dict

class QuizApp:
    def __init__(self, questions_file: str = "questions.json"):
        self.questions = self.load_questions(questions_file)
        self.score = 0
        self.total_questions = 0
        
    def load_questions(self, filename: str) -> List[Dict]:
        """Load questions from JSON file"""
        try:
            with open(filename, 'r', encoding='utf-8') as file:
                return json.load(file)
        except FileNotFoundError:
            print(f"Error: {filename} not found!")
            return []
        except json.JSONDecodeError:
            print(f"Error: Invalid JSON in {filename}")
            return []
    
    def display_question(self, question_data: Dict, question_num: int):
        """Display a single question with its options"""
        print(f"\n--- Question {question_num} ---")
        print(f"Q: {question_data['question']}")
        print("\nOptions:")
        
        for i, answer in enumerate(question_data['answers']):
            letter = chr(ord('a') + i)  # Convert 0,1,2,3 to a,b,c,d
            print(f"  {letter}) {answer}")
    
    def get_user_answer(self) -> str:
        """Get and validate user answer"""
        while True:
            answer = input("\nYour answer (enter letter(s), e.g., 'a' or 'abc'): ").lower().strip()
            
            if not answer:
                print("Please enter an answer.")
                continue
                
            # Validate that all characters are valid letters
            valid_letters = set('abcdefgh')  # Support up to 8 options
            if all(c in valid_letters for c in answer):
                # Sort the letters to match the format in JSON
                return ''.join(sorted(set(answer)))
            else:
                print("Please enter valid letters (a-h).")
    
    def check_answer(self, user_answer: str, correct_answer: str) -> bool:
        """Check if user answer matches correct answer"""
        return user_answer == correct_answer
    
    def run_quiz(self, randomize: bool = False):
        """Run the complete quiz"""
        if not self.questions:
            print("No questions available!")
            return
        
        print("=" * 50)
        print("🧠 AI QUIZ APPLICATION 🧠")
        print("=" * 50)
        print("Instructions:")
        print("- Answer with the letter(s) of correct option(s)")
        print("- For multiple correct answers, enter all letters (e.g., 'abc')")
        print("- Type 'quit' to exit early")
        print("=" * 50)
        
        questions_to_ask = self.questions.copy()
        if randomize:
            random.shuffle(questions_to_ask)
        
        self.score = 0
        self.total_questions = len(questions_to_ask)
        
        for i, question in enumerate(questions_to_ask, 1):
            self.display_question(question, i)
            
            user_answer = self.get_user_answer()
            
            if user_answer == 'quit':
                print("\nQuiz terminated early.")
                self.total_questions = i - 1
                break
            
            correct_answer = question['correct']
            is_correct = self.check_answer(user_answer, correct_answer)
            
            if is_correct:
                print("✅ Correct!")
                self.score += 1
            else:
                print(f"❌ Incorrect. The correct answer was: {correct_answer}")
            
            # Show progress
            print(f"Score: {self.score}/{i}")
        
        self.show_results()
    
    def show_results(self):
        """Display final quiz results"""
        print("\n" + "=" * 50)
        print("🎯 QUIZ RESULTS 🎯")
        print("=" * 50)
        
        percentage = (self.score / self.total_questions * 100) if self.total_questions > 0 else 0
        
        print(f"Questions answered: {self.total_questions}")
        print(f"Correct answers: {self.score}")
        print(f"Incorrect answers: {self.total_questions - self.score}")
        print(f"Score: {percentage:.1f}%")
        
        # Performance message
        if percentage >= 90:
            print("🏆 Excellent! Outstanding performance!")
        elif percentage >= 80:
            print("🎉 Great job! Very good understanding!")
        elif percentage >= 70:
            print("👍 Good work! Room for improvement.")
        elif percentage >= 60:
            print("📚 Not bad, but consider reviewing the material.")
        else:
            print("📖 Keep studying! You'll do better next time.")
        
        print("=" * 50)

def main():
    """Main function to run the quiz application"""
    quiz = QuizApp()
    
    if not quiz.questions:
        return
    
    print(f"Loaded {len(quiz.questions)} questions successfully!")
    
    while True:
        print("\nQuiz Options:")
        print("1. Start quiz (in order)")
        print("2. Start quiz (randomized)")
        print("3. Quit")
        
        choice = input("\nSelect an option (1-3): ").strip()
        
        if choice == '1':
            quiz.run_quiz(randomize=False)
        elif choice == '2':
            quiz.run_quiz(randomize=True)
        elif choice == '3':
            print("Goodbye! Happy studying! 📚")
            break
        else:
            print("Invalid choice. Please select 1, 2, or 3.")

if __name__ == "__main__":
    main()
