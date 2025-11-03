#!/bin/bash

echo "======================================"
echo "Matrix Multiplication Threading Lab"
echo "======================================"
echo ""

# Check if jenv is being used
if command -v jenv &> /dev/null; then
    echo "Using jenv for Java version management"
    echo "Current Java version:"
    jenv version
    echo ""
fi

# Check Java installation
if ! command -v java &> /dev/null; then
    echo "ERROR: Java is not installed or not in PATH"
    exit 1
fi

if ! command -v javac &> /dev/null; then
    echo "ERROR: javac is not installed or not in PATH"
    exit 1
fi

echo "Java version:"
java -version
echo ""

# Compile
echo "Compiling Java files..."
javac MatrixMultiply.java Benchmark.java

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed!"
    exit 1
fi

echo "✓ Compilation successful!"
echo ""

# Show menu
echo "What would you like to run?"
echo "1) Quick test (1000x1000 matrix, 4 threads)"
echo "2) Debug mode (9x9 matrix with thread output)"
echo "3) Full benchmark suite"
echo "4) Custom parameters"
echo ""
read -p "Enter choice [1-4]: " choice

case $choice in
    1)
        echo ""
        echo "Running quick test..."
        java MatrixMultiply 1000 1000 1000 4
        ;;
    2)
        echo ""
        echo "Running debug mode..."
        java MatrixMultiply 9 9 9 4 debug
        ;;
    3)
        echo ""
        echo "Running full benchmark (this may take a while)..."
        java Benchmark
        ;;
    4)
        echo ""
        read -p "Enter rows for matrix A: " rows
        read -p "Enter cols for matrix A (rows for matrix B): " cols
        read -p "Enter cols for matrix B: " bcols
        read -p "Enter number of threads: " threads
        read -p "Enable debug? (yes/no): " debug
        
        if [ "$debug" = "yes" ]; then
            java MatrixMultiply $rows $cols $bcols $threads debug
        else
            java MatrixMultiply $rows $cols $bcols $threads
        fi
        ;;
    *)
        echo "Invalid choice!"
        exit 1
        ;;
esac

echo ""
echo "Done!"