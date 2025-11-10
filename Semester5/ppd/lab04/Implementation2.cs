using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;

namespace HttpDownloader;

/// <summary>
/// Implementation 2: Task-Based with ContinueWith (Bonus)
/// Wraps BeginConnect/EndConnect operations in Tasks
/// Chains tasks using ContinueWith()
/// </summary>
public static class Implementation2
{
    private const int BufferSize = 4096;

    public static void DownloadAll(HttpRequest[] requests)
    {
        Console.WriteLine("\n=== Implementation 2: Task-Based (ContinueWith Continuations) ===\n");

        var startTime = DateTime.Now;

        var tasks = new Task[requests.Length];
        for (int i = 0; i < requests.Length; i++)
        {
            tasks[i] = DownloadAsync(requests[i]);
        }

        Task.WaitAll(tasks);
        var totalTime = (DateTime.Now - startTime).TotalMilliseconds;
        Console.WriteLine($"\nAll downloads completed in {totalTime:F0} ms\n");
    }

    private static Task DownloadAsync(HttpRequest request)
    {
        Console.WriteLine($"Starting download: {request}");
        var startTime = DateTime.Now;

        return ConnectAsync(request)
            .ContinueWith(connectTask =>
            {
                if (connectTask.IsFaulted)
                {
                    Console.WriteLine($"Connect failed for {request.Id}: {connectTask.Exception?.GetBaseException().Message}");
                    return Task.FromResult("");
                }

                var socket = connectTask.Result;
                return SendRequestAsync(socket, request)
                    .ContinueWith(sendTask =>
                    {
                        if (sendTask.IsFaulted)
                        {
                            Console.WriteLine($"Send failed for {request.Id}: {sendTask.Exception?.GetBaseException().Message}");
                            CloseSocket(socket);
                            return Task.FromResult("");
                        }

                        return ReceiveResponseAsync(socket)
                            .ContinueWith(receiveTask =>
                            {
                                CloseSocket(socket);

                                if (receiveTask.IsFaulted)
                                {
                                    Console.WriteLine($"Receive failed for {request.Id}: {receiveTask.Exception?.GetBaseException().Message}");
                                    return "";
                                }

                                return receiveTask.Result;
                            });
                    }).Unwrap();
            }).Unwrap()
            .ContinueWith(responseTask =>
            {
                if (!responseTask.IsFaulted && !string.IsNullOrEmpty(responseTask.Result))
                {
                    var downloadTime = (DateTime.Now - startTime).TotalMilliseconds;
                    var parsed = HttpParser.Parse(responseTask.Result, request.Id);
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
            });
    }

    private static Task<Socket> ConnectAsync(HttpRequest request)
    {
        var tcs = new TaskCompletionSource<Socket>();

        try
        {
            var hostEntry = Dns.GetHostEntry(request.Host);
            var socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
            var endpoint = new IPEndPoint(hostEntry.AddressList[0], request.Port);

            socket.BeginConnect(endpoint, ar =>
            {
                try
                {
                    socket.EndConnect(ar);
                    tcs.SetResult(socket);
                }
                catch (Exception ex)
                {
                    CloseSocket(socket);
                    tcs.SetException(ex);
                }
            }, null);
        }
        catch (Exception ex)
        {
            tcs.SetException(ex);
        }

        return tcs.Task;
    }

    private static Task SendRequestAsync(Socket socket, HttpRequest request)
    {
        var tcs = new TaskCompletionSource<bool>();

        try
        {
            var requestText = request.BuildHttpRequest();
            var requestBytes = Encoding.UTF8.GetBytes(requestText);

            socket.BeginSend(requestBytes, 0, requestBytes.Length, SocketFlags.None, ar =>
            {
                try
                {
                    socket.EndSend(ar);
                    tcs.SetResult(true);
                }
                catch (Exception ex)
                {
                    tcs.SetException(ex);
                }
            }, null);
        }
        catch (Exception ex)
        {
            tcs.SetException(ex);
        }

        return tcs.Task;
    }

    private static Task<string> ReceiveResponseAsync(Socket socket)
    {
        var tcs = new TaskCompletionSource<string>();
        var content = new StringBuilder();
        var buffer = new byte[BufferSize];

        void ReceiveChunk()
        {
            try
            {
                socket.BeginReceive(buffer, 0, BufferSize, SocketFlags.None, ar =>
                {
                    try
                    {
                        var bytesReceived = socket.EndReceive(ar);

                        if (bytesReceived == 0)
                        {
                            // End of stream
                            tcs.SetResult(content.ToString());
                            return;
                        }

                        var chunk = Encoding.UTF8.GetString(buffer, 0, bytesReceived);
                        content.Append(chunk);

                        // Continue receiving
                        ReceiveChunk();
                    }
                    catch (SocketException)
                    {
                        // Connection closed
                        tcs.SetResult(content.ToString());
                    }
                    catch (Exception ex)
                    {
                        tcs.SetException(ex);
                    }
                }, null);
            }
            catch (Exception ex)
            {
                tcs.SetException(ex);
            }
        }

        ReceiveChunk();
        return tcs.Task;
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