namespace HttpDownloader;

public static class HttpParser
{
    public static ParsedResponse Parse(string rawResponse, string requestId)
    {
        if (string.IsNullOrEmpty(rawResponse))
        {
            return new ParsedResponse(requestId, 0, "Empty Response", -1, "", "");
        }

        var parts = rawResponse.Split(new[] { "\r\n\r\n" }, 2, StringSplitOptions.None);
        var headerSection = parts[0];
        var body = parts.Length > 1 ? parts[1] : "";

        var headerLines = headerSection.Split(new[] { "\r\n" }, StringSplitOptions.None);
        if (headerLines.Length == 0)
        {
            return new ParsedResponse(requestId, 0, "Invalid Response", -1, "", body);
        }

        // Parse status line: HTTP/1.1 200 OK
        var statusLine = headerLines[0];
        int statusCode = 0;
        string statusMessage = "";

        var statusParts = statusLine.Split(new[] { ' ' }, 3);
        if (statusParts.Length >= 2)
        {
            int.TryParse(statusParts[1], out statusCode);
            statusMessage = statusParts.Length >= 3 ? statusParts[2] : "";
        }

        // Parse headers and find Content-Length
        int contentLength = -1;
        var headersBuilder = new System.Text.StringBuilder();

        for (int i = 1; i < headerLines.Length; i++)
        {
            var line = headerLines[i];
            headersBuilder.AppendLine(line);

            if (line.StartsWith("Content-Length:", StringComparison.OrdinalIgnoreCase))
            {
                var value = line.Substring(15).Trim();
                int.TryParse(value, out contentLength);
            }
        }

        return new ParsedResponse(requestId, statusCode, statusMessage,
                                 contentLength, headersBuilder.ToString(), body);
    }

    public class ParsedResponse
    {
        public string Id { get; }
        public int StatusCode { get; }
        public string StatusMessage { get; }
        public int ContentLength { get; }
        public string Headers { get; }
        public string Body { get; }

        public ParsedResponse(string id, int statusCode, string statusMessage,
                            int contentLength, string headers, string body)
        {
            Id = id;
            StatusCode = statusCode;
            StatusMessage = statusMessage;
            ContentLength = contentLength;
            Headers = headers;
            Body = body;
        }
    }
}