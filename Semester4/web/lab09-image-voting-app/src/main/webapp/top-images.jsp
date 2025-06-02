<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Top Images - Image Voting App</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background-color: #f8f9fa;
            line-height: 1.6;
        }
        
        .navbar {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 1rem 0;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .navbar-content {
            max-width: 1200px;
            margin: 0 auto;
            padding: 0 1rem;
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        
        .navbar h1 {
            font-size: 1.5rem;
        }
        
        .navbar-nav {
            display: flex;
            gap: 1rem;
            align-items: center;
        }
        
        .navbar-nav a {
            color: white;
            text-decoration: none;
            padding: 0.5rem 1rem;
            border-radius: 5px;
            transition: background-color 0.3s;
        }
        
        .navbar-nav a:hover {
            background-color: rgba(255,255,255,0.1);
        }
        
        .container {
            max-width: 1200px;
            margin: 2rem auto;
            padding: 0 1rem;
        }
        
        .page-header {
            background: white;
            padding: 2rem;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            margin-bottom: 2rem;
            text-align: center;
        }
        
        .page-header h2 {
            color: #333;
            margin-bottom: 1rem;
        }
        
        .limit-selector {
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 1rem;
            margin-top: 1rem;
        }
        
        .limit-selector label {
            color: #666;
            font-weight: 500;
        }
        
        .limit-selector select {
            padding: 0.5rem;
            border: 2px solid #e1e5e9;
            border-radius: 5px;
            font-size: 1rem;
        }
        
        .limit-selector button {
            padding: 0.5rem 1rem;
            background: #667eea;
            color: white;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            font-size: 0.9rem;
        }
        
        .limit-selector button:hover {
            background: #5a6fd8;
        }
        
        .images-section {
            background: white;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        
        .section-header {
            background: #f8f9fa;
            padding: 1rem 1.5rem;
            border-bottom: 1px solid #e9ecef;
        }
        
        .section-header h3 {
            color: #333;
            margin: 0;
        }
        
        .image-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
            gap: 1.5rem;
            padding: 1.5rem;
        }
        
        .image-card {
            border: 1px solid #e9ecef;
            border-radius: 10px;
            overflow: hidden;
            transition: transform 0.3s, box-shadow 0.3s;
            position: relative;
        }
        
        .image-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 5px 20px rgba(0,0,0,0.1);
        }
        
        .rank-badge {
            position: absolute;
            top: 10px;
            left: 10px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 0.5rem;
            border-radius: 50%;
            font-weight: bold;
            font-size: 1.1rem;
            min-width: 40px;
            height: 40px;
            display: flex;
            align-items: center;
            justify-content: center;
            box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        }
        
        .rank-badge.gold {
            background: linear-gradient(135deg, #ffd700 0%, #ffed4e 100%);
            color: #333;
        }
        
        .rank-badge.silver {
            background: linear-gradient(135deg, #c0c0c0 0%, #e8e8e8 100%);
            color: #333;
        }
        
        .rank-badge.bronze {
            background: linear-gradient(135deg, #cd7f32 0%, #daa520 100%);
            color: white;
        }
        
        .image-card img {
            width: 100%;
            height: 250px;
            object-fit: cover;
        }
        
        .image-info {
            padding: 1rem;
        }
        
        .image-title {
            font-weight: bold;
            margin-bottom: 0.5rem;
            color: #333;
            font-size: 1.1rem;
        }
        
        .image-meta {
            font-size: 0.9rem;
            color: #666;
            margin-bottom: 0.5rem;
        }
        
        .image-description {
            color: #555;
            margin-bottom: 1rem;
            font-size: 0.9rem;
        }
        
        .vote-stats {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding-top: 1rem;
            border-top: 1px solid #e9ecef;
        }
        
        .total-votes {
            font-weight: bold;
            color: #667eea;
            font-size: 1.1rem;
        }
        
        .vote-icon {
            color: #28a745;
        }
        
        .no-images {
            text-align: center;
            color: #666;
            padding: 3rem;
        }
        
        .no-images h3 {
            margin-bottom: 1rem;
            color: #999;
        }
        
        .no-images p {
            margin-bottom: 1.5rem;
        }
        
        .no-images a {
            color: #667eea;
            text-decoration: none;
            font-weight: 500;
        }
        
        .no-images a:hover {
            text-decoration: underline;
        }
        
        .stats-summary {
            background: #f8f9fa;
            padding: 1rem 1.5rem;
            border-bottom: 1px solid #e9ecef;
            font-size: 0.9rem;
            color: #666;
        }
    </style>
</head>
<body>
    <nav class="navbar">
        <div class="navbar-content">
            <h1>Image Voting App</h1>
            <div class="navbar-nav">
                <span>Welcome, ${username}!</span>
                <a href="${pageContext.request.contextPath}/dashboard">Dashboard</a>
                <a href="${pageContext.request.contextPath}/upload">Upload Image</a>
                <a href="${pageContext.request.contextPath}/logout">Logout</a>
            </div>
        </div>
    </nav>
    
    <div class="container">
        <div class="page-header">
            <h2>🏆 Top Voted Images</h2>
            <p>Discover the most popular images in our community</p>
            
            <div class="limit-selector">
                <label for="limitSelect">Show top:</label>
                <select id="limitSelect">
                    <option value="5" ${limit == 5 ? 'selected' : ''}>5 images</option>
                    <option value="10" ${limit == 10 ? 'selected' : ''}>10 images</option>
                    <option value="20" ${limit == 20 ? 'selected' : ''}>20 images</option>
                    <option value="50" ${limit == 50 ? 'selected' : ''}>50 images</option>
                </select>
                <button onclick="updateLimit()">Update</button>
            </div>
        </div>
        
        <div class="images-section">
            <div class="section-header">
                <h3>Top ${limit} Images by Votes</h3>
            </div>
            
            <c:if test="${not empty topImages}">
                <div class="stats-summary">
                    Showing ${topImages.size()} of top ${limit} images
                </div>
            </c:if>
            
            <c:choose>
                <c:when test="${not empty topImages}">
                    <div class="image-grid">
                        <c:forEach var="image" items="${topImages}" varStatus="status">
                            <div class="image-card">
                                <c:choose>
                                    <c:when test="${status.index == 0}">
                                        <div class="rank-badge gold">${status.index + 1}</div>
                                    </c:when>
                                    <c:when test="${status.index == 1}">
                                        <div class="rank-badge silver">${status.index + 1}</div>
                                    </c:when>
                                    <c:when test="${status.index == 2}">
                                        <div class="rank-badge bronze">${status.index + 1}</div>
                                    </c:when>
                                    <c:otherwise>
                                        <div class="rank-badge">${status.index + 1}</div>
                                    </c:otherwise>
                                </c:choose>
                                
                                <img src="${pageContext.request.contextPath}/images/${image.filename}" 
                                     alt="${image.title}">
                                
                                <div class="image-info">
                                    <div class="image-title">${image.title}</div>
                                    <div class="image-meta">
                                        By: ${image.uploaderUsername} | 
                                        <fmt:formatDate value="${image.uploadDate}" pattern="MMM dd, yyyy"/>
                                    </div>
                                    
                                    <c:if test="${not empty image.description}">
                                        <div class="image-description">${image.description}</div>
                                    </c:if>
                                    
                                    <div class="vote-stats">
                                        <div class="total-votes">
                                            <span class="vote-icon">⭐</span> ${image.totalVotes} votes
                                        </div>
                                        <c:if test="${image.userId == currentUserId}">
                                            <span style="color: #28a745; font-size: 0.8rem;">Your image</span>
                                        </c:if>
                                    </div>
                                </div>
                            </div>
                        </c:forEach>
                    </div>
                </c:when>
                <c:otherwise>
                    <div class="no-images">
                        <h3>No Images Found</h3>
                        <p>There are no images to display yet.</p>
                        <a href="${pageContext.request.contextPath}/upload">Be the first to upload an image!</a>
                    </div>
                </c:otherwise>
            </c:choose>
        </div>
    </div>
    
    <script>
        function updateLimit() {
            const limit = document.getElementById('limitSelect').value;
            window.location.href = '${pageContext.request.contextPath}/top-images?limit=' + limit;
        }
        
        // Add keyboard shortcut for updating
        document.getElementById('limitSelect').addEventListener('keypress', function(e) {
            if (e.key === 'Enter') {
                updateLimit();
            }
        });
    </script>
</body>
</html> 