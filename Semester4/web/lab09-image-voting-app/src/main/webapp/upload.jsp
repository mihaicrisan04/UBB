<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Upload Image - Image Voting App</title>
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
            max-width: 600px;
            margin: 2rem auto;
            padding: 0 1rem;
        }
        
        .upload-card {
            background: white;
            padding: 2rem;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .upload-header {
            text-align: center;
            margin-bottom: 2rem;
        }
        
        .upload-header h2 {
            color: #333;
            margin-bottom: 0.5rem;
        }
        
        .upload-header p {
            color: #666;
            font-size: 0.9rem;
        }
        
        .form-group {
            margin-bottom: 1.5rem;
        }
        
        .form-group label {
            display: block;
            margin-bottom: 0.5rem;
            color: #333;
            font-weight: 500;
        }
        
        .form-group input,
        .form-group textarea {
            width: 100%;
            padding: 0.75rem;
            border: 2px solid #e1e5e9;
            border-radius: 5px;
            font-size: 1rem;
            transition: border-color 0.3s;
            font-family: inherit;
        }
        
        .form-group input:focus,
        .form-group textarea:focus {
            outline: none;
            border-color: #667eea;
        }
        
        .form-group textarea {
            resize: vertical;
            min-height: 100px;
        }
        
        .file-input-wrapper {
            position: relative;
            display: inline-block;
            width: 100%;
        }
        
        .file-input {
            width: 100%;
            padding: 0.75rem;
            border: 2px dashed #e1e5e9;
            border-radius: 5px;
            background: #f8f9fa;
            text-align: center;
            cursor: pointer;
            transition: all 0.3s;
        }
        
        .file-input:hover {
            border-color: #667eea;
            background: #f0f0ff;
        }
        
        .file-input input[type="file"] {
            position: absolute;
            opacity: 0;
            width: 100%;
            height: 100%;
            cursor: pointer;
        }
        
        .file-preview {
            margin-top: 1rem;
            text-align: center;
        }
        
        .file-preview img {
            max-width: 100%;
            max-height: 200px;
            border-radius: 5px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .btn {
            width: 100%;
            padding: 0.75rem;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            border: none;
            border-radius: 5px;
            font-size: 1rem;
            cursor: pointer;
            transition: transform 0.2s;
        }
        
        .btn:hover {
            transform: translateY(-2px);
        }
        
        .btn:disabled {
            background: #ccc;
            cursor: not-allowed;
            transform: none;
        }
        
        .alert {
            padding: 0.75rem;
            margin-bottom: 1rem;
            border-radius: 5px;
            font-size: 0.9rem;
        }
        
        .alert-error {
            background-color: #f8d7da;
            color: #721c24;
            border: 1px solid #f5c6cb;
        }
        
        .alert-success {
            background-color: #d4edda;
            color: #155724;
            border: 1px solid #c3e6cb;
        }
        
        .form-help {
            font-size: 0.8rem;
            color: #666;
            margin-top: 0.25rem;
        }
        
        .back-link {
            text-align: center;
            margin-top: 1.5rem;
            padding-top: 1.5rem;
            border-top: 1px solid #e1e5e9;
        }
        
        .back-link a {
            color: #667eea;
            text-decoration: none;
        }
        
        .back-link a:hover {
            text-decoration: underline;
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
                <a href="${pageContext.request.contextPath}/top-images">Top Images</a>
                <a href="${pageContext.request.contextPath}/logout">Logout</a>
            </div>
        </div>
    </nav>
    
    <div class="container">
        <div class="upload-card">
            <div class="upload-header">
                <h2>Upload New Image</h2>
                <p>Share your image with the community</p>
            </div>
            
            <!-- Display messages -->
            <c:if test="${not empty error}">
                <div class="alert alert-error">
                    ${error}
                </div>
            </c:if>
            
            <c:if test="${not empty success}">
                <div class="alert alert-success">
                    ${success}
                </div>
            </c:if>
            
            <form action="${pageContext.request.contextPath}/upload" method="post" 
                  enctype="multipart/form-data" id="uploadForm">
                
                <div class="form-group">
                    <label for="title">Title:</label>
                    <input type="text" id="title" name="title" required maxlength="100"
                           placeholder="Enter a descriptive title for your image">
                    <div class="form-help">Maximum 100 characters</div>
                </div>
                
                <div class="form-group">
                    <label for="description">Description (Optional):</label>
                    <textarea id="description" name="description" maxlength="500"
                              placeholder="Describe your image or tell us about it..."></textarea>
                    <div class="form-help">Maximum 500 characters</div>
                </div>
                
                <div class="form-group">
                    <label for="image">Select Image:</label>
                    <div class="file-input-wrapper">
                        <div class="file-input">
                            <input type="file" id="image" name="image" accept="image/*" required>
                            <div id="file-text">
                                Click here to select an image or drag and drop
                                <br><small>Supported formats: JPG, PNG, GIF, BMP (Max 10MB)</small>
                            </div>
                        </div>
                    </div>
                    <div class="file-preview" id="preview" style="display: none;">
                        <img id="preview-img" src="" alt="Preview">
                    </div>
                </div>
                
                <button type="submit" class="btn" id="submitBtn">Upload Image</button>
            </form>
            
            <div class="back-link">
                <a href="${pageContext.request.contextPath}/dashboard">← Back to Dashboard</a>
            </div>
        </div>
    </div>
    
    <script>
        const fileInput = document.getElementById('image');
        const fileText = document.getElementById('file-text');
        const preview = document.getElementById('preview');
        const previewImg = document.getElementById('preview-img');
        const submitBtn = document.getElementById('submitBtn');
        
        fileInput.addEventListener('change', function(e) {
            const file = e.target.files[0];
            
            if (file) {
                // Validate file size (10MB)
                if (file.size > 10 * 1024 * 1024) {
                    alert('File size must be less than 10MB');
                    fileInput.value = '';
                    return;
                }
                
                // Validate file type
                const allowedTypes = ['image/jpeg', 'image/jpg', 'image/png', 'image/gif', 'image/bmp'];
                if (!allowedTypes.includes(file.type)) {
                    alert('Please select a valid image file (JPG, PNG, GIF, BMP)');
                    fileInput.value = '';
                    return;
                }
                
                // Update file text
                fileText.innerHTML = '<strong>Selected:</strong> ' + file.name;
                
                // Show preview
                const reader = new FileReader();
                reader.onload = function(e) {
                    previewImg.src = e.target.result;
                    preview.style.display = 'block';
                };
                reader.readAsDataURL(file);
            } else {
                fileText.innerHTML = 'Click here to select an image or drag and drop<br><small>Supported formats: JPG, PNG, GIF, BMP (Max 10MB)</small>';
                preview.style.display = 'none';
            }
        });
        
        // Form submission handling
        document.getElementById('uploadForm').addEventListener('submit', function(e) {
            submitBtn.disabled = true;
            submitBtn.textContent = 'Uploading...';
        });
        
        // Drag and drop functionality
        const fileInputWrapper = document.querySelector('.file-input');
        
        fileInputWrapper.addEventListener('dragover', function(e) {
            e.preventDefault();
            this.style.borderColor = '#667eea';
            this.style.background = '#f0f0ff';
        });
        
        fileInputWrapper.addEventListener('dragleave', function(e) {
            e.preventDefault();
            this.style.borderColor = '#e1e5e9';
            this.style.background = '#f8f9fa';
        });
        
        fileInputWrapper.addEventListener('drop', function(e) {
            e.preventDefault();
            this.style.borderColor = '#e1e5e9';
            this.style.background = '#f8f9fa';
            
            const files = e.dataTransfer.files;
            if (files.length > 0) {
                fileInput.files = files;
                fileInput.dispatchEvent(new Event('change'));
            }
        });
    </script>
</body>
</html> 