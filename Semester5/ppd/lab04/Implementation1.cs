using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace HttpDownloader;

/// <summary>
/// Implementation 1: Event-Driven (Direct Callbacks)
/// Uses Socket.BeginConnect/EndConnect, BeginSend/EndSend, BeginReceive/EndReceive
/// Parser integrated directly in callbacks
/// </summary>
public static class Implementation1
{
    private const int BufferSize = 4096;

    public static void DownloadAll(HttpRequest[] requests)
    {
        Console.WriteLine("\n=== Implementation 1: Event-Driven (Direct Callbacks) ===\n");

        var startTime = DateTime.Now;
        var countdown = new CountdownEvent(requests.Length);

        foreach (var request in requests)
        {
            DownloadAsync(request, countdown);
        }

        countdown.Wait();
        var totalTime = (DateTime.Now - startTime).TotalMilliseconds;
        Console.WriteLine($"\nAll downloads completed in {totalTime:F0} ms\n");
    }

    private static void DownloadAsync(HttpRequest request, CountdownEvent countdown)
    {
        try
        {
            Console.WriteLine($"Starting download: {request}");

            var hostEntry = Dns.GetHostEntry(request.Host);
            var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            var endpoint = new IPEndPoint(hostEntry.AddressList[0], request.Port);
            var state = new DownloadState(socket, request, countdown);

            socket.BeginConnect(endpoint, ConnectCallback, state);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Failed to start download for {request.Id}: {ex.Message}");
            countdown.Signal();
        }
    }

    private static void ConnectCallback(IAsyncResult ar)
    {
        var state = (DownloadState)ar.AsyncState!;
        
        try
        {
            state.Socket.EndConnect(ar);

            var requestText = state.Request.BuildHttpRequest();
            var requestBytes = Encoding.UTF8.GetBytes(requestText);

            state.Socket.BeginSend(requestBytes, 0, requestBytes.Length, SocketFlags.None, SendCallback, state);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Connect failed for {state.Request.Id}: {ex.Message}");
            CloseSocket(state);
        }
    }

    private static void SendCallback(IAsyncResult ar)
    {
        var state = (DownloadState)ar.AsyncState!;
        
        try
        {
            var bytesSent = state.Socket.EndSend(ar);

            state.Socket.BeginReceive(state.Buffer, 0, BufferSize, SocketFlags.None, ReceiveCallback, state);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Send failed for {state.Request.Id}: {ex.Message}");
            CloseSocket(state);
        }
    }

    private static void ReceiveCallback(IAsyncResult ar)
    {
        var state = (DownloadState)ar.AsyncState!;
        
        try
        {
            var bytesReceived = state.Socket.EndReceive(ar);

            if (bytesReceived == 0)
            {
                // End of stream - parse and display
                ParseAndDisplay(state);
                CloseSocket(state);
                return;
            }

            var chunk = Encoding.UTF8.GetString(state.Buffer, 0, bytesReceived);
            state.Content.Append(chunk);

            // Continue receiving
            state.Socket.BeginReceive(state.Buffer, 0, BufferSize, SocketFlags.None, ReceiveCallback, state);
        }
        catch (SocketException)
        {
            // Connection closed - parse what we have
            ParseAndDisplay(state);
            CloseSocket(state);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Receive failed for {state.Request.Id}: {ex.Message}");
            CloseSocket(state);
        }
    }

    private static void ParseAndDisplay(DownloadState state)
    {
        var downloadTime = (DateTime.Now - state.StartTime).TotalMilliseconds;
        var rawResponse = state.Content.ToString();

        var parsed = HttpParser.Parse(rawResponse, state.Request.Id);
        var response = new HttpResponse(
            parsed.Id,
            parsed.StatusCode,
            parsed.StatusMessage,
            parsed.ContentLength,
            parsed.Headers,
            parsed.Body,
            (long)downloadTime
        );

        Console.WriteLine(response);
    }

    private static void CloseSocket(DownloadState state)
    {
        try
        {
            if (state.Socket.Connected)
                state.Socket.Shutdown(SocketShutdown.Both);
            state.Socket.Close();
        }
        catch
        {
            // Ignore
        }
        finally
        {
            state.Countdown.Signal();
        }
    }

    private class DownloadState
    {
        public Socket Socket { get; }
        public HttpRequest Request { get; }
        public CountdownEvent Countdown { get; }
        public byte[] Buffer { get; }
        public StringBuilder Content { get; }
        public DateTime StartTime { get; }

        public DownloadState(Socket socket, HttpRequest request, CountdownEvent countdown)
        {
            Socket = socket;
            Request = request;
            Countdown = countdown;
            Buffer = new byte[BufferSize];
            Content = new StringBuilder();
            StartTime = DateTime.Now;
        }
    }
}