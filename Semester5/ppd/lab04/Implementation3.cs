using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;

namespace HttpDownloader;

/// <summary>
/// Implementation 3: Async/Await Mechanism
/// Uses async/await with Task-based wrappers around Begin/End methods
/// Most readable and maintainable approach
/// </summary>
public static class Implementation3
{
    private const int BufferSize = 4096;

    public static async Task DownloadAllAsync(HttpRequest[] requests)
    {
        Console.WriteLine("\n=== Implementation 3: Async/Await Mechanism ===\n");

        var startTime = DateTime.Now;

        var tasks = new Task[requests.Length];
        for (int i = 0; i < requests.Length; i++)
        {
            tasks[i] = DownloadAsync(requests[i]);
        }

        await Task.WhenAll(tasks);
        var totalTime = (DateTime.Now - startTime).TotalMilliseconds;
        Console.WriteLine($"\nAll downloads completed in {totalTime:F0} ms\n");
    }

    private static async Task DownloadAsync(HttpRequest request)
    {
        Console.WriteLine($"Starting download: {request}");
        var startTime = DateTime.Now;

        Socket? socket = null;
        try
        {
            socket = await ConnectAsync(request);
            await SendRequestAsync(socket, request);
            var response = await ReceiveResponseAsync(socket);

            var downloadTime = (DateTime.Now - startTime).TotalMilliseconds;
            var parsed = HttpParser.Parse(response, request.Id);
            var httpResponse = new HttpResponse(
                parsed.Id,
                parsed.StatusCode,
                parsed.StatusMessage,
                parsed.ContentLength,
                parsed.Headers,
                parsed.Body,
                (long)downloadTime
            );

            Console.WriteLine(httpResponse);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Download failed for {request.Id}: {ex.Message}");
        }
        finally
        {
            if (socket != null)
                CloseSocket(socket);
        }
    }

    private static async Task<Socket> ConnectAsync(HttpRequest request)
    {
        var hostEntry = await Dns.GetHostEntryAsync(request.Host);
        var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        var endpoint = new IPEndPoint(hostEntry.AddressList[0], request.Port);

        await Task.Factory.FromAsync(
            socket.BeginConnect(endpoint, null, null),
            socket.EndConnect
        );

        return socket;
    }

    private static async Task SendRequestAsync(Socket socket, HttpRequest request)
    {
        var requestText = request.BuildHttpRequest();
        var requestBytes = Encoding.UTF8.GetBytes(requestText);

        await Task.Factory.FromAsync(
            socket.BeginSend(requestBytes, 0, requestBytes.Length, SocketFlags.None, null, null),
            socket.EndSend
        );
    }

    private static async Task<string> ReceiveResponseAsync(Socket socket)
    {
        var content = new StringBuilder();
        var buffer = new byte[BufferSize];

        while (true)
        {
            var bytesReceived = await Task.Factory.FromAsync(
                socket.BeginReceive(buffer, 0, BufferSize, SocketFlags.None, null, null),
                socket.EndReceive
            );

            if (bytesReceived == 0)
                break;

            var chunk = Encoding.UTF8.GetString(buffer, 0, bytesReceived);
            content.Append(chunk);
        }

        return content.ToString();
    }

    private static void CloseSocket(Socket socket)
    {
        try
        {
            if (socket.Connected)
                socket.Shutdown(SocketShutdown.Both);
            socket.Close();
        }
        catch
        {
            // Ignore
        }
    }
}