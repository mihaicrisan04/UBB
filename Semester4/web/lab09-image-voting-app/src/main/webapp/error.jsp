<%@ page contentType="text/html;charset=UTF-8" language="java" isErrorPage="true" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Error - Image Voting App</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 1rem;
        }
        
        .error-container {
            background: white;
            padding: 3rem;
            border-radius: 15px;
            box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
            text-align: center;
            max-width: 500px;
            width: 100%;
        }
        
        .error-icon {
            font-size: 4rem;
            margin-bottom: 1rem;
        }
        
        .error-code {
            font-size: 3rem;
            font-weight: bold;
            color: #667eea;
            margin-bottom: 1rem;
        }
        
        .error-title {
            font-size: 1.5rem;
            color: #333;
            margin-bottom: 1rem;
        }
        
        .error-message {
            color: #666;
            margin-bottom: 2rem;
            line-height: 1.6;
        }
        
        .error-actions {
            display: flex;
            gap: 1rem;
            justify-content: center;
            flex-wrap: wrap;
        }
        
        .btn {
            padding: 0.75rem 1.5rem;
            border: none;
            border-radius: 5px;
            text-decoration: none;
            font-size: 1rem;
            cursor: pointer;
            transition: transform 0.2s;
            display: inline-block;
        }
        
        .btn:hover {
            transform: translateY(-2px);
        }
        
        .btn-primary {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
        }
        
        .btn-secondary {
            background: #f8f9fa;
            color: #333;
            border: 2px solid #e9ecef;
        }
        
        .error-details {
            margin-top: 2rem;
            padding-top: 2rem;
            border-top: 1px solid #e9ecef;
            font-size: 0.9rem;
            color: #999;
        }
        
        .error-details summary {
            cursor: pointer;
            font-weight: 500;
            margin-bottom: 1rem;
        }
        
        .error-details pre {
            background: #f8f9fa;
            padding: 1rem;
            border-radius: 5px;
            text-align: left;
            overflow-x: auto;
            font-size: 0.8rem;
        }
    </style>
</head>
<body>
    <div class="error-container">
        <c:choose>
            <c:when test="${pageContext.errorData.statusCode == 404}">
                <div class="error-icon">🔍</div>
                <div class="error-code">404</div>
                <div class="error-title">Page Not Found</div>
                <div class="error-message">
                    Sorry, the page you are looking for doesn't exist or has been moved.
                </div>
            </c:when>
            <c:when test="${pageContext.errorData.statusCode == 500}">
                <div class="error-icon">⚠️</div>
                <div class="error-code">500</div>
                <div class="error-title">Internal Server Error</div>
                <div class="error-message">
                    Something went wrong on our end. Please try again later or contact support if the problem persists.
                </div>
            </c:when>
            <c:otherwise>
                <div class="error-icon">❌</div>
                <div class="error-code">Error</div>
                <div class="error-title">Something Went Wrong</div>
                <div class="error-message">
                    An unexpected error occurred. Please try again or go back to the homepage.
                </div>
            </c:otherwise>
        </c:choose>
        
        <div class="error-actions">
            <a href="${pageContext.request.contextPath}/dashboard" class="btn btn-primary">
                Go to Dashboard
            </a>
            <a href="javascript:history.back()" class="btn btn-secondary">
                Go Back
            </a>
        </div>
        
        <c:if test="${pageContext.errorData != null}">
            <div class="error-details">
                <details>
                    <summary>Technical Details</summary>
                    <p><strong>Status Code:</strong> ${pageContext.errorData.statusCode}</p>
                    <p><strong>Request URI:</strong> ${pageContext.errorData.requestURI}</p>
                    <c:if test="${pageContext.errorData.throwable != null}">
                        <p><strong>Exception:</strong> ${pageContext.errorData.throwable.class.name}</p>
                        <c:if test="${pageContext.errorData.throwable.message != null}">
                            <p><strong>Message:</strong> ${pageContext.errorData.throwable.message}</p>
                        </c:if>
                    </c:if>
                </details>
            </div>
        </c:if>
    </div>
</body>
</html> 