# Lab 4 - Asynchronous HTTP Downloader (C#)

## Overview

This lab implements asynchronous HTTP downloads in C# using non-blocking I/O and demonstrates three different programming approaches with futures and continuations. The program downloads multiple files simultaneously using low-level Socket APIs.

## Lab Requirements

**Goal**: Use C# TPL futures and continuations in a complex scenario with external events.

**Task**: Download several files simultaneously through HTTP using:
- `BeginConnect()/EndConnect()`
- `BeginSend()/EndSend()`
- `BeginReceive()/EndReceive()` Socket functions
- Simple HTTP protocol parser (status line, headers, Content-Length)

## Three Implementations

### Implementation 1: Event-Driven (Direct Callbacks)
- **Pattern**: Traditional callback-based async I/O
- **Method**: Parser integrated directly in callbacks
- **APIs**: `Socket.BeginConnect/EndConnect`, callback methods
- **Pros**: Direct, minimal overhead, easy to understand flow
- **Cons**: Callback nesting, scattered state management

### Implementation 2: Task-Based with ContinueWith() ⭐ BONUS (2p)
- **Pattern**: Functional composition with Task continuations
- **Method**: Wrap Begin/End in `TaskCompletionSource`, chain with `ContinueWith()`
- **APIs**: `Task<T>`, `TaskCompletionSource<T>`, `ContinueWith()`
- **Pros**: Composable, clear separation, error propagation
- **Cons**: More boilerplate, requires understanding continuations

### Implementation 3: Async/Await Mechanism
- **Pattern**: Sequential-style async code
- **Method**: `async/await` with `Task.Factory.FromAsync()`
- **APIs**: `async`, `await`, `Task.WhenAll()`
- **Pros**: Most readable, maintainable, linear flow
- **Cons**: Hides async complexity (but that's usually good!)

## Project Structure

```
lab04/
├── HttpDownloader.csproj       # .NET project file
├── Program.cs                  # Main entry point
├── HttpRequest.cs              # HTTP request model
├── HttpResponse.cs             # HTTP response model
├── HttpParser.cs               # Simple HTTP/1.1 parser
├── Implementation1.cs          # Event-driven (callbacks)
├── Implementation2.cs          # Task-based (ContinueWith)
├── Implementation3.cs          # Async/await
├── Makefile                    # Build automation
├── run.sh                      # Convenience script
├── README.md                   # This file
├── COMPARISON.md               # Detailed implementation comparison
├── Lab4_CSharp_Reference.cs    # Original reference code
└── .gitignore                  # Git ignore patterns
```

## Prerequisites

- **.NET SDK 6.0 or later** (tested with .NET 8.0)
- **make** (optional, for using Makefile)
- **bash** (optional, for using run.sh)

## Installation Tutorial

### Step 1: Install .NET SDK

**macOS:**
```bash
# Using Homebrew
brew install --cask dotnet-sdk

# Or download from:
# https://dotnet.microsoft.com/download
```

**Linux:**
```bash
# Ubuntu/Debian
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
./dotnet-install.sh --channel 8.0

# Or use package manager
sudo apt-get install dotnet-sdk-8.0
```

**Windows:**
Download and install from: https://dotnet.microsoft.com/download

### Step 2: Verify Installation

```bash
dotnet --version
# Should show: 6.0.x, 7.0.x, or 8.0.x
```

### Step 3: Navigate to Project

```bash
cd ppd/lab04
```

## Building and Running

### Option 1: Using Makefile (Recommended)

**Build the project:**
```bash
make build
# or just
make
```

**Run all implementations:**
```bash
make run
```

**Run specific implementation:**
```bash
make run1    # Event-driven (callbacks)
make run2    # Task-based (ContinueWith)
make run3    # Async/await
```

**Clean build artifacts:**
```bash
make clean
```

**Rebuild from scratch:**
```bash
make rebuild
```

**Show help:**
```bash
make help
```

### Option 2: Using run.sh Script

```bash
# Make it executable (first time only)
chmod +x run.sh

# Run all implementations
./run.sh
./run.sh all

# Run specific implementation
./run.sh 1    # Event-driven
./run.sh 2    # Task-based
./run.sh 3    # Async/await
```

### Option 3: Using dotnet CLI Directly

**Build:**
```bash
dotnet build
```

**Run all implementations:**
```bash
dotnet run
```

**Run specific implementation:**
```bash
dotnet run -- 1    # Event-driven
dotnet run -- 2    # Task-based
dotnet run -- 3    # Async/await
```

**Clean:**
```bash
dotnet clean
```

**Build in Release mode:**
```bash
dotnet build -c Release
dotnet run -c Release
```

## Example Output

```
HTTP Downloader - Asynchronous Download Implementations
========================================================

=== Implementation 1: Event-Driven (Direct Callbacks) ===

Starting download: Example: http://www.example.com:80/
Starting download: HTTPBin-HTML: http://httpbin.org:80/html
Starting download: HTTPBin-JSON: http://httpbin.org:80/json
[Example] Status: 200 OK, Content-Length: 1256, Actual: 1256 bytes, Time: 234 ms
[HTTPBin-HTML] Status: 200 OK, Content-Length: 3741, Actual: 3741 bytes, Time: 312 ms
[HTTPBin-JSON] Status: 200 OK, Content-Length: 429, Actual: 429 bytes, Time: 298 ms

All downloads completed in 315 ms

============================================================

=== Implementation 2: Task-Based (ContinueWith Continuations) ===

Starting download: Example: http://www.example.com:80/
Starting download: HTTPBin-HTML: http://httpbin.org:80/html
Starting download: HTTPBin-JSON: http://httpbin.org:80/json
[Example] Status: 200 OK, Content-Length: 1256, Actual: 1256 bytes, Time: 189 ms
[HTTPBin-JSON] Status: 200 OK, Content-Length: 429, Actual: 429 bytes, Time: 245 ms
[HTTPBin-HTML] Status: 200 OK, Content-Length: 3741, Actual: 3741 bytes, Time: 267 ms

All downloads completed in 271 ms

============================================================

=== Implementation 3: Async/Await Mechanism ===

Starting download: Example: http://www.example.com:80/
Starting download: HTTPBin-HTML: http://httpbin.org:80/html
Starting download: HTTPBin-JSON: http://httpbin.org:80/json
[Example] Status: 200 OK, Content-Length: 1256, Actual: 1256 bytes, Time: 156 ms
[HTTPBin-HTML] Status: 200 OK, Content-Length: 3741, Actual: 3741 bytes, Time: 234 ms
[HTTPBin-JSON] Status: 200 OK, Content-Length: 429, Actual: 429 bytes, Time: 221 ms

All downloads completed in 237 ms


All tests completed!
```

## Key Features

✅ **Non-blocking I/O**: Uses asynchronous Socket operations  
✅ **Concurrent Downloads**: Multiple URLs downloaded simultaneously  
✅ **No Wait() Blocking**: Except in Main() to wait for completion  
✅ **HTTP/1.1 Parser**: Parses status, headers, Content-Length  
✅ **Error Handling**: Graceful handling of network failures  
✅ **Performance Metrics**: Tracks individual and total download times  
✅ **Three Approaches**: Event-driven, Task-based, Async/await  
✅ **Clean Build**: All artifacts in bin/ and obj/ directories  

## HTTP Parser Details

The simple HTTP parser can:
- Extract status code and message from the status line
- Parse HTTP headers line by line
- Find and extract the `Content-Length` header value
- Separate headers from body content
- Handle basic HTTP/1.1 responses

**Example parsed response:**
```
HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 1256
Connection: close

<html>...</html>
```

## Customizing Test URLs

Edit `Program.cs` and modify the `CreateTestRequests()` method:

```csharp
private static HttpRequest[] CreateTestRequests()
{
    return new[]
    {
        new HttpRequest("www.example.com", "/", "Example"),
        new HttpRequest("httpbin.org", "/html", "HTTPBin-HTML"),
        new HttpRequest("your-site.com", "/path", "YourSite"),
        // Add more...
    };
}
```

## Troubleshooting

### "dotnet: command not found"
- Install .NET SDK (see Installation Tutorial above)
- Check PATH: `echo $PATH` should include dotnet location
- Restart terminal after installation

### Build errors
```bash
# Try restoring NuGet packages
dotnet restore

# Clean and rebuild
make rebuild
# or
dotnet clean && dotnet build
```

### Connection timeout/errors
- Check your internet connection
- Some URLs might be blocked by firewall
- Try with `www.example.com` first (most reliable)

### "No such host is known" error
- DNS resolution issue
- Try pinging the host: `ping www.example.com`
- Check your network connection

## Performance Notes

- All three implementations use the same async I/O, so performance is similar
- Network latency dominates execution time
- Concurrent downloads are truly parallel (non-blocking)
- Implementation 3 (async/await) is usually fastest due to TPL optimizations

## Comparison with Java Version

This C# implementation:
- Uses native TPL (Task Parallel Library) instead of CompletableFuture
- Has more concise syntax with async/await
- Uses `TaskCompletionSource<T>` for wrapping callbacks
- Supports true async/await at language level
- Has better integration with the .NET runtime

## Learning Resources

- [Asynchronous Programming with async/await](https://docs.microsoft.com/en-us/dotnet/csharp/async)
- [Task-based Asynchronous Pattern (TAP)](https://docs.microsoft.com/en-us/dotnet/standard/asynchronous-programming-patterns/task-based-asynchronous-pattern-tap)
- [Socket Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.socket)
- [TaskCompletionSource](https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks.taskcompletionsource-1)

## Quick Reference

| Command | Description |
|---------|-------------|
| `make` | Build project |
| `make run` | Run all implementations |
| `make run1` | Run implementation 1 |
| `make run2` | Run implementation 2 |
| `make run3` | Run implementation 3 |
| `make clean` | Clean build artifacts |
| `make rebuild` | Clean and rebuild |
| `make help` | Show help |
| `./run.sh` | Run all (alternative) |
| `./run.sh 1` | Run impl 1 (alternative) |
| `dotnet build` | Build (direct) |
| `dotnet run` | Run (direct) |
| `dotnet run -- 1` | Run impl 1 (direct) |

## License

Educational project for PPD (Parallel and Distributed Programming) course.