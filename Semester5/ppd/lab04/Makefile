# Makefile for C# HTTP Downloader Lab
DOTNET = dotnet
PROJECT = HttpDownloader.csproj
BUILD_CONFIG = Release

# Default target
all: build

# Build the project
build:
	@echo "Building project..."
	$(DOTNET) build $(PROJECT) -c $(BUILD_CONFIG)

# Run all implementations
run: build
	$(DOTNET) run --no-build -c $(BUILD_CONFIG)

# Run specific implementation (make run1, run2, run3)
run1: build
	$(DOTNET) run --no-build -c $(BUILD_CONFIG) -- 1

run2: build
	$(DOTNET) run --no-build -c $(BUILD_CONFIG) -- 2

run3: build
	$(DOTNET) run --no-build -c $(BUILD_CONFIG) -- 3

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	$(DOTNET) clean $(PROJECT)
	rm -rf bin obj

# Rebuild everything
rebuild: clean build

# Restore NuGet packages
restore:
	$(DOTNET) restore $(PROJECT)

# Help
help:
	@echo "HTTP Downloader Makefile"
	@echo "========================"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Build the project"
	@echo "  make build    - Build the project"
	@echo "  make run      - Run all implementations"
	@echo "  make run1     - Run implementation 1 (event-driven)"
	@echo "  make run2     - Run implementation 2 (task-based with ContinueWith)"
	@echo "  make run3     - Run implementation 3 (async/await)"
	@echo "  make clean    - Remove build artifacts (bin/ and obj/)"
	@echo "  make rebuild  - Clean and rebuild"
	@echo "  make restore  - Restore NuGet packages"
	@echo "  make help     - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make run1     - Run event-driven implementation"
	@echo "  make run      - Run all three implementations"
	@echo ""
	@echo "Note: Compiled files are in bin/$(BUILD_CONFIG)/"

.PHONY: all build run run1 run2 run3 clean rebuild restore help