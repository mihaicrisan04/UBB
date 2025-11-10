#!/bin/bash

# HTTP Downloader - Convenience runner script for C#
# Usage: ./run.sh [1|2|3|all]

set -e

DOTNET=dotnet
PROJECT="HttpDownloader.csproj"
BUILD_CONFIG="Release"

# Check if dotnet is installed
if ! command -v dotnet &> /dev/null; then
    echo "Error: dotnet CLI is not installed or not in PATH"
    echo "Please install .NET SDK from https://dotnet.microsoft.com/download"
    exit 1
fi

# Build if needed
if [ ! -d "bin/$BUILD_CONFIG" ]; then
    echo "Building project..."
    $DOTNET build $PROJECT -c $BUILD_CONFIG
    echo ""
fi

# Run implementation
if [ $# -eq 0 ]; then
    # No arguments - run all
    $DOTNET run --no-build -c $BUILD_CONFIG
elif [ "$1" = "all" ]; then
    $DOTNET run --no-build -c $BUILD_CONFIG
elif [ "$1" = "1" ]; then
    $DOTNET run --no-build -c $BUILD_CONFIG -- 1
elif [ "$1" = "2" ]; then
    $DOTNET run --no-build -c $BUILD_CONFIG -- 2
elif [ "$1" = "3" ]; then
    $DOTNET run --no-build -c $BUILD_CONFIG -- 3
else
    echo "Usage: $0 [1|2|3|all]"
    echo "  1   - Run implementation 1 (event-driven)"
    echo "  2   - Run implementation 2 (task-based with ContinueWith)"
    echo "  3   - Run implementation 3 (async/await)"
    echo "  all - Run all implementations (default)"
    exit 1
fi