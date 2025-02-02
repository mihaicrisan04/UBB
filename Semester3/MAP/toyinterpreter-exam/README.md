# Toy Language Interpreter with GUI

This is a JavaFX-based GUI implementation of a Toy Language Interpreter. The project is built using Maven and follows standard Maven project structure.

## Project Structure

- `src/main/java/com/example/`
  - `App.java` - Main JavaFX application
  - `controller/` - Program control logic
  - `model/` - Core language components
    - `expressions/` - Arithmetic and logical expressions
    - `statements/` - Program statements
    - `types/` - Data types
    - `values/` - Value implementations
  - `collections/` - Custom data structures
  - `repository/` - Program state storage

## Usage

1. When you run the application, you'll see a window with a list of predefined programs
2. Select a program from the list and click "Select Program"
3. The main execution window will open showing:
   - Number of program states
   - Heap table
   - Output
   - File table
   - Program state IDs
   - Symbol table
   - Execution stack
4. Click "Run one step" to execute the program step by step
5. The UI updates after each step to show the current state

## Build and Run

Navigate to the project directory:

```bash
cd toyinterpreter
```

To build the project, run the following command:

```bash
mvn clean install
```

To run the project, run the following command:

```bash
mvn javafx:run
```

## Example Programs

The interpreter comes with several example programs demonstrating different features:

1. Simple arithmetic and printing
2. If statements and boolean operations
3. While loops
4. File operations
5. Heap allocation and manipulation
6. Concurrent execution with Fork
7. Reference types and garbage collection

## Troubleshooting

- If you get compilation errors, ensure you're using Java 17 or higher
- For JavaFX errors, verify that your IDE has the JavaFX SDK properly configured
- Check that Maven can access the internet to download dependencies
- Ensure the logs directory exists for program execution logs

## Implementation Details

- The interpreter uses a custom stack implementation for program execution
- Garbage collection is implemented using a conservative mark-and-sweep algorithm
- Concurrent execution is handled through Java's ExecutorService
- Type checking is performed before program execution
- Program state is maintained in a repository pattern
