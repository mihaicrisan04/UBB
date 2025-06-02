<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="fmt" uri="http://java.sun.com/jsp/jstl/fmt" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Dashboard - Image Voting App</title>
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
        
        .section {
            background: white;
            margin-bottom: 2rem;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            overflow: hidden;
        }
        
        .section-header {
            background: #f8f9fa;
            padding: 1rem 1.5rem;
            border-bottom: 1px solid #e9ecef;
        }
        
        .section-header h2 {
            color: #333;
            margin: 0;
        }
        
        .section-content {
            padding: 1.5rem;
        }
        
        .image-grid {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
            gap: 1.5rem;
        }
        
        .image-card {
            border: 1px solid #e9ecef;
            border-radius: 10px;
            overflow: hidden;
            transition: transform 0.3s, box-shadow 0.3s;
        }
        
        .image-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 5px 20px rgba(0,0,0,0.1);
        }
        
        .image-card img {
            width: 100%;
            height: 200px;
            object-fit: cover;
        }
        
        .image-info {
            padding: 1rem;
        }
        
        .image-title {
            font-weight: bold;
            margin-bottom: 0.5rem;
            color: #333;
        }
        
        .image-meta {
            font-size: 0.9rem;
            color: #666;
            margin-bottom: 0.5rem;
        }
        
        .image-description {
            color: #555;
            margin-bottom: 1rem;
        }
        
        .vote-section {
            display: flex;
            align-items: center;
            gap: 1rem;
            padding-top: 1rem;
            border-top: 1px solid #e9ecef;
        }
        
        .vote-input {
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }
        
        .vote-input input {
            width: 60px;
            padding: 0.25rem;
            border: 1px solid #ddd;
            border-radius: 3px;
            text-align: center;
        }
        
        .vote-btn {
            background: #28a745;
            color: white;
            border: none;
            padding: 0.5rem 1rem;
            border-radius: 5px;
            cursor: pointer;
            font-size: 0.9rem;
        }
        
        .vote-btn:hover {
            background: #218838;
        }
        
        .vote-stats {
            font-weight: bold;
            color: #667eea;
        }
        
        .no-images {
            text-align: center;
            color: #666;
            padding: 2rem;
        }
        
        .alert {
            padding: 0.75rem 1rem;
            margin-bottom: 1rem;
            border-radius: 5px;
            font-size: 0.9rem;
        }
        
        .alert-success {
            background-color: #d4edda;
            color: #155724;
            border: 1px solid #c3e6cb;
        }
        
        .tabs {
            display: flex;
            background: #f8f9fa;
            border-bottom: 1px solid #e9ecef;
        }
        
        .tab {
            padding: 1rem 1.5rem;
            cursor: pointer;
            border-bottom: 3px solid transparent;
            transition: all 0.3s;
        }
        
        .tab.active {
            background: white;
            border-bottom-color: #667eea;
            color: #667eea;
        }
        
        .tab-content {
            display: none;
            padding: 1.5rem;
        }
        
        .tab-content.active {
            display: block;
        }
    </style>
</head>
<body>
    <nav class="navbar">
        <div class="navbar-content">
            <h1>Image Voting App</h1>
            <div class="navbar-nav">
                <span>Welcome, ${username}!</span>
                <a href="${pageContext.request.contextPath}/upload">Upload Image</a>
                <a href="${pageContext.request.contextPath}/top-images">Top Images</a>
                <a href="${pageContext.request.contextPath}/logout">Logout</a>
            </div>
        </div>
    </nav>
    
    <div class="container">
        <c:if test="${param.success == 'upload'}">
            <div class="alert alert-success">
                Image uploaded successfully!
            </div>
        </c:if>
        
        <div class="section">
            <div class="tabs">
                <div class="tab active" onclick="showTab('all-images')">All Images</div>
                <div class="tab" onclick="showTab('votable-images')">Images to Vote</div>
                <div class="tab" onclick="showTab('my-images')">My Images</div>
            </div>
            
            <!-- All Images Tab -->
            <div id="all-images" class="tab-content active">
                <c:choose>
                    <c:when test="${not empty allImages}">
                        <div class="image-grid">
                            <c:forEach var="image" items="${allImages}">
                                <div class="image-card">
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
                                        <div class="vote-section">
                                            <div class="vote-stats">Votes: ${image.totalVotes}</div>
                                        </div>
                                    </div>
                                </div>
                            </c:forEach>
                        </div>
                    </c:when>
                    <c:otherwise>
                        <div class="no-images">No images uploaded yet.</div>
                    </c:otherwise>
                </c:choose>
            </div>
            
            <!-- Votable Images Tab -->
            <div id="votable-images" class="tab-content">
                <c:choose>
                    <c:when test="${not empty votableImages}">
                        <div class="image-grid">
                            <c:forEach var="image" items="${votableImages}">
                                <div class="image-card">
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
                                        <div class="vote-section">
                                            <div class="vote-stats">Current Votes: ${image.totalVotes}</div>
                                            <div class="vote-input">
                                                <input type="number" min="1" max="10" value="5" 
                                                       id="vote-${image.id}">
                                                <button class="vote-btn" 
                                                        onclick="submitVote('${image.id}')">Vote</button>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            </c:forEach>
                        </div>
                    </c:when>
                    <c:otherwise>
                        <div class="no-images">No images available for voting.</div>
                    </c:otherwise>
                </c:choose>
            </div>
            
            <!-- My Images Tab -->
            <div id="my-images" class="tab-content">
                <c:choose>
                    <c:when test="${not empty userImages}">
                        <div class="image-grid">
                            <c:forEach var="image" items="${userImages}">
                                <div class="image-card">
                                    <img src="${pageContext.request.contextPath}/images/${image.filename}" 
                                         alt="${image.title}">
                                    <div class="image-info">
                                        <div class="image-title">${image.title}</div>
                                        <div class="image-meta">
                                            Uploaded: <fmt:formatDate value="${image.uploadDate}" pattern="MMM dd, yyyy"/>
                                        </div>
                                        <c:if test="${not empty image.description}">
                                            <div class="image-description">${image.description}</div>
                                        </c:if>
                                        <div class="vote-section">
                                            <div class="vote-stats">Total Votes: ${image.totalVotes}</div>
                                        </div>
                                    </div>
                                </div>
                            </c:forEach>
                        </div>
                    </c:when>
                    <c:otherwise>
                        <div class="no-images">
                            You haven't uploaded any images yet. 
                            <a href="${pageContext.request.contextPath}/upload">Upload your first image!</a>
                        </div>
                    </c:otherwise>
                </c:choose>
            </div>
        </div>
    </div>
    
    <script>
        function showTab(tabName) {
            // Hide all tab contents
            const tabContents = document.querySelectorAll('.tab-content');
            tabContents.forEach(content => content.classList.remove('active'));
            
            // Remove active class from all tabs
            const tabs = document.querySelectorAll('.tab');
            tabs.forEach(tab => tab.classList.remove('active'));
            
            // Show selected tab content
            document.getElementById(tabName).classList.add('active');
            
            // Add active class to clicked tab
            const clickedTab = document.querySelector("[onclick=\"showTab('" + tabName + "')\"]");
            if (clickedTab) {
                clickedTab.classList.add('active');
            }
        }
        
        function submitVote(imageId) {
            const voteValue = document.getElementById('vote-' + imageId).value;
            
            if (!voteValue || voteValue < 1 || voteValue > 10) {
                alert('Please enter a vote value between 1 and 10');
                return;
            }
            
            fetch('${pageContext.request.contextPath}/vote', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                },
                body: 'imageId=' + imageId + '&voteValue=' + voteValue
            })
            .then(response => response.json())
            .then(data => {
                if (data.success) {
                    alert('Vote submitted successfully!');
                    location.reload(); // Refresh to update vote counts
                } else {
                    alert('Error: ' + data.message);
                }
            })
            .catch(error => {
                console.error('Error:', error);
                alert('An error occurred while submitting your vote');
            });
        }
    </script>
</body>
</html> 