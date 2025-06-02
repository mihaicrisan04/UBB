package com.imagevoting.servlet;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;

public class ImageServlet extends HttpServlet {
    
    private static final String UPLOAD_DIR = "uploads";
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        // Get the image filename from the URL path
        String pathInfo = request.getPathInfo();
        
        if (pathInfo == null || pathInfo.length() <= 1) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }
        
        // Remove leading slash
        String filename = pathInfo.substring(1);
        
        // Validate filename to prevent directory traversal attacks
        if (filename.contains("..") || filename.contains("/") || filename.contains("\\")) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }
        
        // Get the file path
        String uploadPath = getServletContext().getRealPath("") + File.separator + UPLOAD_DIR;
        File imageFile = new File(uploadPath, filename);
        
        // Check if file exists
        if (!imageFile.exists() || !imageFile.isFile()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }
        
        // Set content type based on file extension
        String contentType = getContentType(filename);
        if (contentType != null) {
            response.setContentType(contentType);
        }
        
        // Set content length
        response.setContentLength((int) imageFile.length());
        
        // Set cache headers for better performance
        response.setHeader("Cache-Control", "public, max-age=3600"); // Cache for 1 hour
        
        // Stream the file to the response
        try (FileInputStream fileInputStream = new FileInputStream(imageFile);
             OutputStream outputStream = response.getOutputStream()) {
            
            byte[] buffer = new byte[4096];
            int bytesRead;
            
            while ((bytesRead = fileInputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            
            outputStream.flush();
        } catch (IOException e) {
            System.err.println("Error serving image: " + e.getMessage());
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }
    
    /**
     * Get MIME content type based on file extension
     */
    private String getContentType(String filename) {
        if (filename == null) {
            return null;
        }
        
        String lowerCaseFilename = filename.toLowerCase();
        
        if (lowerCaseFilename.endsWith(".jpg") || lowerCaseFilename.endsWith(".jpeg")) {
            return "image/jpeg";
        } else if (lowerCaseFilename.endsWith(".png")) {
            return "image/png";
        } else if (lowerCaseFilename.endsWith(".gif")) {
            return "image/gif";
        } else if (lowerCaseFilename.endsWith(".bmp")) {
            return "image/bmp";
        } else if (lowerCaseFilename.endsWith(".webp")) {
            return "image/webp";
        }
        
        return "application/octet-stream"; // Default binary content type
    }
} 