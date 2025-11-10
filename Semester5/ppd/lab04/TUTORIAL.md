# Lab 4 - HTTP Downloader Tutorial

## Complete Step-by-Step Guide for Running the C# Project

This tutorial will guide you through setting up, building, and running the HTTP Downloader project from scratch.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [Project Setup](#project-setup)
4. [Building the Project](#building-the-project)
5. [Running the Project](#running-the-project)
6. [Understanding the Output](#understanding-the-output)
7. [Troubleshooting](#troubleshooting)
8. [Advanced Usage](#advanced-usage)

---

## Prerequisites

### What You Need

- **.NET SDK** (version 6.0 or later)
- **Terminal/Command Prompt** access
- **Internet connection** (for downloading test files)
- **make** (optional, but recommended)

### Checking If You Have .NET

Open a terminal and run:

```bash
dotnet --version
```

**Expected output:**
```
8.0.121
```
(or 6.0.x, 7.0.x, etc.)

**If you get "command not found"**, you need to install .NET SDK (see next section).

---

## Installation

### macOS

#### Option 1: Using Homebrew (Recommended)

```bash
# Install Homebrew if you don't have it
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install .NET SDK
brew install --cask dotnet-sdk

# Verify installation
dotnet --version
```

#### Option 2: Direct Download

1. Visit: https://dotnet.microsoft.com/download
2. Download ".NET SDK" for macOS
3. Run the installer (.pkg file)
4. Restart your terminal
5. Verify: `dotnet --version`

### Linux (Ubuntu/Debian)

#### Using the Microsoft repository:

```bash
# Download the install script
wget https://dot.net/v1/dotnet-install.sh

# Make it executable
chmod +x dotnet-install.sh

# Install .NET 8.0
./dotnet-install.sh --channel 8.0

# Add to PATH (add to ~/.bashrc or ~/.zshrc)
export DOTNET_ROOT=$HOME/.dotnet
export PATH=$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools

# Reload shell configuration
source ~/.bashrc  # or source ~/.zshrc

# Verify installation
dotnet --version
```

#### Using apt (Ubuntu 20.04+):

```bash
# Add Microsoft package repository
wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

# Update package list
sudo apt-get update

# Install .NET SDK
sudo apt-get install -y dotnet-sdk-8.0

# Verify installation
dotnet --version
```

### Windows

1. Visit: https://dotnet.microsoft.com/download
2. Download ".NET SDK" for Windows
3. Run the installer (.exe file)
4. Follow the installation wizard
5. Open a new Command Prompt or PowerShell
6. Verify: `dotnet --version`

---

## Project Setup

### Navigate to the Project Directory

```bash
cd ~/Developer/UBB/Semester5/ppd/lab04
```

### Verify Project Structure

```bash
ls -la
```

**You should see:**
- `HttpDownloader.csproj` - Project file
- `Program.cs` - Main entry point
- `Implementation1.cs`, `Implementation2.cs`, `Implementation3.cs` - Three implementations
- `HttpRequest.cs`, `HttpResponse.cs`, `HttpParser.cs` - Model classes
- `Makefile` - Build automation
- `run.sh` - Convenience script
- `README.md`, `COMPARISON.md` - Documentation

### Restore Dependencies (Optional)

While this happens automatically during build, you can do it explicitly:

```bash
dotnet restore
```

**Expected output:**
```
Determining projects to restore...
Restored /path/to/HttpDownloader.csproj (in 123 ms).
```

---

## Building the Project

You have **three options** to build the project. Choose one:

### Option 1: Using Makefile (Recommended)

```bash
make
```

or

```bash
make build
```

**Output:**
```
Building project...
dotnet build HttpDownloader.csproj -c Release
...
Build succeeded.
    0 Warning(s)
    0 Error(s)
```

### Option 2: Using dotnet CLI Directly

```bash
dotnet build
```

For optimized release build:

```bash
dotnet build -c Release
```

### Option 3: Build is Automatic

When you run the project, it will build automatically if needed.

---

## Running the Project

### Quick Start - Run All Implementations

Using Makefile:
```bash
make run
```

Using run.sh:
```bash
./run.sh
```

Using dotnet:
```bash
dotnet run
```

### Run Specific Implementation

#### Implementation 1: Event-Driven (Callbacks)

**Using Makefile:**
```bash
make run1
```

**Using run.sh:**
```bash
./run.sh 1
```

**Using dotnet:**
```bash
dotnet run -- 1
```

#### Implementation 2: Task-Based (ContinueWith)

**Using Makefile:**
```bash
make run2
```

**Using run.sh:**
```bash
./run.sh 2
```

**Using dotnet:**
```bash
dotnet run -- 2
```

#### Implementation 3: Async/Await

**Using Makefile:**
```bash
make run3
```

**Using run.sh:**
```bash
./run.sh 3
```

**Using dotnet:**
```bash
dotnet run -- 3
```

---

## Understanding the Output

### Example Run

When you run the project, you'll see output like this:

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

### Breaking Down the Output

**Header:**
```
HTTP Downloader - Asynchronous Download Implementations
========================================================
```
- Project title and separator

**Implementation Section:**
```
=== Implementation 1: Event-Driven (Direct Callbacks) ===
```
- Shows which implementation is running

**Download Start Messages:**
```
Starting download: Example: http://www.example.com:80/
```
- Shows when each download begins
- All three start concurrently

**Completion Messages:**
```
[Example] Status: 200 OK, Content-Length: 1256, Actual: 1256 bytes, Time: 234 ms
```
- `[Example]` - Request ID
- `Status: 200 OK` - HTTP status code and message
- `Content-Length: 1256` - Size from HTTP header
- `Actual: 1256 bytes` - Actually downloaded bytes
- `Time: 234 ms` - Download duration

**Summary:**
```
All downloads completed in 315 ms
```
- Total time for all concurrent downloads

### HTTP Status Codes

- **200 OK** - Success
- **301/302** - Redirect (you might see these)
- **404** - Not Found
- **500** - Server Error

---

## Troubleshooting

### Issue 1: "dotnet: command not found"

**Problem:** .NET SDK is not installed or not in PATH

**Solution:**
1. Install .NET SDK (see Installation section)
2. Restart your terminal
3. Verify: `dotnet --version`

### Issue 2: Build Fails with "The SDK 'Microsoft.NET.Sdk' specified could not be found"

**Problem:** .NET SDK installation is incomplete

**Solution:**
```bash
# Reinstall .NET SDK
# macOS:
brew reinstall --cask dotnet-sdk

# Or download from microsoft.com and reinstall
```

### Issue 3: "No such host is known" Error

**Problem:** DNS resolution failure or no internet connection

**Solution:**
1. Check internet connection: `ping google.com`
2. Check DNS: `nslookup www.example.com`
3. Try with a different URL in the code
4. Check firewall settings

### Issue 4: Connection Timeout

**Problem:** Network firewall or slow connection

**Solution:**
1. Wait longer (some downloads might be slow)
2. Try from a different network
3. Test with `www.example.com` only (most reliable)

### Issue 5: "Permission denied" when running run.sh

**Problem:** Script is not executable

**Solution:**
```bash
chmod +x run.sh
./run.sh
```

### Issue 6: Socket Exceptions

**Example:**
```
Download failed for Example: An existing connection was forcibly closed
```

**Solutions:**
- This is normal for some servers that close connections abruptly
- The parser should still work with the downloaded data
- Try different URLs if persistent

### Issue 7: Build Warnings About Nullable References

These warnings from `reference/Lab4_CSharp_Reference.cs` are expected and can be ignored. The reference file is not compiled into the project.

---

## Advanced Usage

### Cleaning Build Artifacts

**Using Makefile:**
```bash
make clean
```

**Using dotnet:**
```bash
dotnet clean
```

**Manual cleanup:**
```bash
rm -rf bin obj
```

### Rebuild from Scratch

**Using Makefile:**
```bash
make rebuild
```

**Manual:**
```bash
make clean
make build
```

### Running in Debug Mode

By default, the Makefile uses Release mode. For debugging:

```bash
dotnet build -c Debug
dotnet run -c Debug
```

### Customizing Test URLs

Edit `Program.cs`:

```csharp
private static HttpRequest[] CreateTestRequests()
{
    return new[]
    {
        new HttpRequest("www.example.com", "/", "Example"),
        new HttpRequest("httpbin.org", "/html", "HTTPBin-HTML"),
        new HttpRequest("httpbin.org", "/json", "HTTPBin-JSON"),
        // Add your own:
        new HttpRequest("www.google.com", "/", "Google"),
    };
}
```

Then rebuild:
```bash
make rebuild
make run
```

### Testing with Single URL

Edit `Program.cs` and change:
```csharp
var requests = CreateTestRequests();
```
to:
```csharp
var requests = CreateSingleRequest();
```

This will test with just `www.example.com`.

### Performance Testing

To test performance with multiple runs:

```bash
# Run each implementation 5 times
for i in {1..5}; do
  echo "Run $i:"
  make run1
  sleep 1
done
```

### Viewing Detailed Response

To see full HTTP response details, you can modify the code to call:

```csharp
Console.WriteLine(response.GetDetailedInfo());
```

This will show:
- Full headers
- Body preview (first 500 characters)
- All response details

---

## Common Commands Reference

| Action | Makefile | run.sh | dotnet CLI |
|--------|----------|--------|------------|
| Build | `make build` | N/A | `dotnet build` |
| Run all | `make run` | `./run.sh` | `dotnet run` |
| Run impl 1 | `make run1` | `./run.sh 1` | `dotnet run -- 1` |
| Run impl 2 | `make run2` | `./run.sh 2` | `dotnet run -- 2` |
| Run impl 3 | `make run3` | `./run.sh 3` | `dotnet run -- 3` |
| Clean | `make clean` | N/A | `dotnet clean` |
| Rebuild | `make rebuild` | N/A | `dotnet clean && dotnet build` |
| Help | `make help` | `./run.sh` (no args) | N/A |

---

## Understanding the Three Implementations

### Implementation 1: Event-Driven

**What it does:**
- Uses traditional callback pattern
- Calls `BeginConnect`, then `ConnectCallback`
- Calls `BeginSend`, then `SendCallback`
- Calls `BeginReceive`, then `ReceiveCallback` (recursively)

**When to use:**
- Learning callback-based async
- Working with legacy code
- Need maximum control

### Implementation 2: Task-Based (BONUS - 2 points)

**What it does:**
- Wraps callbacks in `TaskCompletionSource`
- Chains tasks with `ContinueWith()`
- More composable than callbacks

**When to use:**
- Complex task orchestration
- Working with .NET 4.0+
- Building libraries

### Implementation 3: Async/Await

**What it does:**
- Uses `async` and `await` keywords
- Looks like synchronous code
- Compiler handles complexity

**When to use:**
- Modern C# development
- Best readability
- Recommended for production

---

## Next Steps

1. **Run all implementations** to see the differences
2. **Read COMPARISON.md** for detailed analysis
3. **Modify test URLs** to download from different sites
4. **Experiment with the code** - add logging, error handling, etc.
5. **Review the source code** to understand each implementation

---

## Getting Help

### Documentation
- `README.md` - Project overview and quick start
- `COMPARISON.md` - Detailed implementation comparison
- `TUTORIAL.md` - This file (complete tutorial)

### Online Resources
- [.NET Documentation](https://docs.microsoft.com/en-us/dotnet/)
- [C# Async Programming](https://docs.microsoft.com/en-us/dotnet/csharp/async)
- [Socket Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.sockets.socket)

### Debugging Tips
1. Add `Console.WriteLine()` statements
2. Use breakpoints in IDE (Visual Studio, VS Code, Rider)
3. Check `bin/Release/net8.0/` for compiled output
4. Review error messages carefully

---

## Summary Checklist

- [ ] .NET SDK installed (`dotnet --version` works)
- [ ] Navigated to project directory
- [ ] Project builds successfully (`make build`)
- [ ] Can run all implementations (`make run`)
- [ ] Can run each implementation individually (`make run1`, `make run2`, `make run3`)
- [ ] Understand the output
- [ ] Read the comparison document

**You're all set!** Start experimenting with the code and enjoy learning about asynchronous programming in C#!

---

## Quick Start Commands

```bash
# One-time setup
cd ~/Developer/UBB/Semester5/ppd/lab04
dotnet --version  # Verify .NET is installed

# Build and run
make run

# Or run specific implementation
make run1  # Event-driven
make run2  # Task-based (bonus)
make run3  # Async/await

# Clean up
make clean
```

That's it! Happy coding! 🚀