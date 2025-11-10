using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace HttpDownloader;

class Program
{
    static async Task Main(string[] args)
    {
        Console.WriteLine("HTTP Downloader - Asynchronous Download Implementations");
        Console.WriteLine("========================================================\n");

        var requests = CreateTestRequests();

        if (args.Length > 0 && int.TryParse(args[0], out int implementation))
        {
            await RunImplementation(implementation, requests);
        }
        else
        {
            // Run all implementations
            await RunImplementation(1, requests);
            Console.WriteLine("\n" + new string('=', 60) + "\n");
            
            await RunImplementation(2, requests);
            Console.WriteLine("\n" + new string('=', 60) + "\n");
            
            await RunImplementation(3, requests);
        }

        Console.WriteLine("\nAll tests completed!");
    }

    private static async Task RunImplementation(int impl, HttpRequest[] requests)
    {
        switch (impl)
        {
            case 1:
                Implementation1.DownloadAll(requests);
                break;
            case 2:
                Implementation2.DownloadAll(requests);
                break;
            case 3:
                await Implementation3.DownloadAllAsync(requests);
                break;
            default:
                Console.WriteLine($"Invalid implementation number: {impl}");
                Console.WriteLine("Usage: dotnet run [1|2|3]");
                Console.WriteLine("  1 - Event-driven (callbacks)");
                Console.WriteLine("  2 - Task-based (ContinueWith)");
                Console.WriteLine("  3 - Async/await");
                break;
        }
    }

    private static HttpRequest[] CreateTestRequests()
    {
        return new[]
        {
            new HttpRequest("www.example.com", "/", "Example"),
            new HttpRequest("httpbin.org", "/html", "HTTPBin-HTML"),
            new HttpRequest("httpbin.org", "/json", "HTTPBin-JSON")
        };
    }

    // Alternative test methods for different scenarios
    private static HttpRequest[] CreateSingleRequest()
    {
        return new[]
        {
            new HttpRequest("www.example.com", "/", "Example")
        };
    }

    private static HttpRequest[] CreateExtendedRequests()
    {
        return new[]
        {
            new HttpRequest("www.example.com", "/", "Example"),
            new HttpRequest("httpbin.org", "/html", "HTTPBin-HTML"),
            new HttpRequest("httpbin.org", "/json", "HTTPBin-JSON"),
            new HttpRequest("httpbin.org", "/xml", "HTTPBin-XML"),
            new HttpRequest("httpbin.org", "/robots.txt", "HTTPBin-Robots")
        };
    }
}