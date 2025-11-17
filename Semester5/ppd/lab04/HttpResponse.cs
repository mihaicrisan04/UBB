namespace HttpDownloader;

public class HttpResponse
{
    public string Id { get; }
    public int StatusCode { get; }
    public string StatusMessage { get; }
    public int ContentLength { get; }
    public string Headers { get; }
    public string Body { get; }
    public long DownloadTimeMs { get; }

    public HttpResponse(string id, int statusCode, string statusMessage,
                       int contentLength, string headers, string body, long downloadTimeMs)
    {
        Id = id;
        StatusCode = statusCode;
        StatusMessage = statusMessage;
        ContentLength = contentLength;
        Headers = headers;
        Body = body;
        DownloadTimeMs = downloadTimeMs;
    }

    public int ActualBodyLength => Body?.Length ?? 0;

    public override string ToString()
    {
        return $"[{Id}] Status: {StatusCode} {StatusMessage}, Content-Length: {ContentLength}, " +
               $"Actual: {ActualBodyLength} bytes, Time: {DownloadTimeMs} ms";
    }

    public string GetDetailedInfo()
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine(new string('=', 60));
        sb.AppendLine($"Response for: {Id}");
        sb.AppendLine(new string('=', 60));
        sb.AppendLine($"Status: {StatusCode} {StatusMessage}");
        sb.AppendLine($"Content-Length: {ContentLength} bytes");
        sb.AppendLine($"Actual Body: {ActualBodyLength} bytes");
        sb.AppendLine($"Download Time: {DownloadTimeMs} ms");
        sb.AppendLine(new string('-', 60));
        sb.AppendLine("Headers:");
        sb.AppendLine(Headers);
        sb.AppendLine(new string('-', 60));
        sb.AppendLine("Body Preview (first 500 chars):");
        if (!string.IsNullOrEmpty(Body))
        {
            sb.AppendLine(Body.Substring(0, Math.Min(500, Body.Length)));
            if (Body.Length > 500)
                sb.AppendLine("... (truncated)");
        }
        else
        {
            sb.AppendLine("(empty)");
        }
        sb.AppendLine(new string('=', 60));
        return sb.ToString();
    }
}
