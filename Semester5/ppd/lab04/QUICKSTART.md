# Quick Start Guide - Lab 4 HTTP Downloader

## TL;DR - Get Running in 30 Seconds

```bash
# Navigate to project
cd ppd/lab04

# Verify .NET is installed
dotnet --version

# Run everything
make run

# Or run specific implementation
make run1    # Event-driven
make run2    # Task-based (bonus)
make run3    # Async/await
```

---

## Installation (First Time Only)

### macOS
```bash
brew install --cask dotnet-sdk
```

### Linux
```bash
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
./dotnet-install.sh --channel 8.0
export PATH=$PATH:$HOME/.dotnet
```

### Windows
Download from: https://dotnet.microsoft.com/download

---

## Running the Project

### Method 1: Makefile (Easiest)
```bash
make run      # Run all implementations
make run1     # Event-driven
make run2     # Task-based with ContinueWith (BONUS 2p)
make run3     # Async/await
make clean    # Clean build
make rebuild  # Clean and rebuild
make help     # Show all commands
```

### Method 2: Shell Script
```bash
./run.sh      # Run all
./run.sh 1    # Event-driven
./run.sh 2    # Task-based
./run.sh 3    # Async/await
```

### Method 3: Direct dotnet
```bash
dotnet build              # Build
dotnet run                # Run all
dotnet run -- 1           # Event-driven
dotnet run -- 2           # Task-based
dotnet run -- 3           # Async/await
dotnet clean              # Clean
```

---

## What You'll See

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
```

---

## Project Structure

```
lab04/
├── Program.cs              # Main entry point
├── Implementation1.cs      # Event-driven (callbacks)
├── Implementation2.cs      # Task-based (ContinueWith) ⭐ BONUS
├── Implementation3.cs      # Async/await
├── HttpRequest.cs          # Request model
├── HttpResponse.cs         # Response model
├── HttpParser.cs           # HTTP parser
├── Makefile               # Build automation
├── run.sh                 # Convenience script
├── README.md              # Full documentation
├── TUTORIAL.md            # Detailed tutorial
├── COMPARISON.md          # Implementation comparison
└── QUICKSTART.md          # This file
```

---

## Three Implementations Explained

### 1. Event-Driven (Direct Callbacks)
- Traditional callback pattern
- `BeginConnect/EndConnect`, `BeginSend/EndSend`, `BeginReceive/EndReceive`
- Parser in callbacks
- **Satisfies base lab requirements**

### 2. Task-Based (ContinueWith) ⭐ BONUS
- Wraps callbacks in `TaskCompletionSource`
- Chains with `ContinueWith()`
- Functional composition
- **Worth 2 bonus points**

### 3. Async/Await
- Modern C# pattern
- `async` and `await` keywords
- Most readable
- **Industry standard**

---

## Common Issues

### "dotnet: command not found"
Install .NET SDK (see Installation section above)

### Connection errors
Check internet connection: `ping www.example.com`

### Permission denied on run.sh
```bash
chmod +x run.sh
```

### Build errors
```bash
make clean
make rebuild
```

---

## Customizing Test URLs

Edit `Program.cs`, find `CreateTestRequests()`:

```csharp
private static HttpRequest[] CreateTestRequests()
{
    return new[]
    {
        new HttpRequest("www.example.com", "/", "Example"),
        new HttpRequest("httpbin.org", "/html", "HTTPBin-HTML"),
        new HttpRequest("httpbin.org", "/json", "HTTPBin-JSON"),
        // Add your own:
        new HttpRequest("your-site.com", "/path", "YourSite"),
    };
}
```

Then: `make rebuild && make run`

---

## Cheat Sheet

| Task | Command |
|------|---------|
| Build | `make` or `dotnet build` |
| Run all | `make run` or `./run.sh` |
| Run impl 1 | `make run1` or `./run.sh 1` |
| Run impl 2 | `make run2` or `./run.sh 2` |
| Run impl 3 | `make run3` or `./run.sh 3` |
| Clean | `make clean` |
| Rebuild | `make rebuild` |
| Help | `make help` |

---

## Lab Requirements Checklist

- ✅ Download multiple files simultaneously
- ✅ Use `BeginConnect/EndConnect`, `BeginSend/EndSend`, `BeginReceive/EndReceive`
- ✅ Simple HTTP parser (status, headers, Content-Length)
- ✅ Implementation 1: Event-driven with callbacks
- ✅ Implementation 2: Task-based with ContinueWith (BONUS 2p)
- ✅ Implementation 3: Async/await mechanism
- ✅ No Wait() calls except in Main

---

## Need More Help?

- **Full documentation**: `README.md`
- **Step-by-step guide**: `TUTORIAL.md`
- **Deep comparison**: `COMPARISON.md`
- **This quick ref**: `QUICKSTART.md`

---

## Ready to Run?

```bash
cd ppd/lab04
make run
```

That's it! 🚀
