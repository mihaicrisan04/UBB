using System;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

public static class Lab4 {
    public static void Main()
    {
        var entry = Dns.GetHostEntry(State.Host);
        var socket = new Socket(SocketType.Stream, ProtocolType.Tcp);
        var endpoint = new IPEndPoint(entry.AddressList[0], State.Port);
        var state = new State(socket);
        state.Socket.BeginConnect(endpoint, ConnectCallback, state);
        state.ReceiveDone.WaitOne();
        state.Socket.Close();
    }

    private static void ConnectCallback(IAsyncResult ar)
    {
        var state = (State)ar.AsyncState;
        state.Socket.EndConnect(ar);
        state.ConnectDone.Set();
        var requestText = $"GET /documente-utile/ HTTP/1.1\r\nHost: {State.Host}\r\n\r\n";
        var requestBytes = Encoding.UTF8.GetBytes(requestText);
        state.Socket.BeginSend(requestBytes, 0, requestBytes.Length, SocketFlags.None, SendCallback, state);
    }

    private static void SendCallback(IAsyncResult ar)
    {
        var state = (State)ar.AsyncState;
        var bytesSent = state.Socket.EndSend(ar);
        state.SendDone.Set();
        state.Socket.BeginReceive(state.Buffer, 0, State.BufferLength, SocketFlags.None, ReceiveCallback, state);
    }

    private static void ReceiveCallback(IAsyncResult ar)
    {
        var state = (State)ar.AsyncState;
        var bytesReceived = state.Socket.EndReceive(ar);
        if (bytesReceived == 0)
        {
            Console.WriteLine(state.Content.ToString());
            state.ReceiveDone.Set();
        }
        else
        {
            var responseText = Encoding.UTF8.GetString(state.Buffer, 0, bytesReceived);
            state.Content.Append(responseText);
            state.Socket.BeginReceive(state.Buffer, 0, State.BufferLength, SocketFlags.None, ReceiveCallback,
                state);
        }
    }

    public sealed class State
    {
        public const string Host = "www.cnatdcu.ro";
        public const int Port = 80;
        public const int BufferLength = 1024;
        public readonly byte[] Buffer = new byte[BufferLength];
        public readonly ManualResetEvent ConnectDone = new ManualResetEvent(false);
        public readonly StringBuilder Content = new StringBuilder();
        public readonly ManualResetEvent ReceiveDone = new ManualResetEvent(false);
        public readonly ManualResetEvent SendDone = new ManualResetEvent(false);
        public readonly Socket Socket;

        public State(Socket socket)
        {
            Socket = socket;
        }
    }
  }
