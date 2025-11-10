namespace HttpDownloader;

public class HttpRequest
{
    public string Host { get; }
    public int Port { get; }
    public string Path { get; }
    public string Id { get; }

    public HttpRequest(string host, int port, string path, string id)
    {
        Host = host;
        Port = port;
        Path = path;
        Id = id;
    }

    public HttpRequest(string host, string path, string id) 
        : this(host, 80, path, id)
    {
    }

    public string BuildHttpRequest()
    {
        return $"GET {Path} HTTP/1.1\r\nHost: {Host}\r\nConnection: close\r\n\r\n";
    }

    public override string ToString()
    {
        return $"{Id}: http://{Host}:{Port}{Path}";
    }
}