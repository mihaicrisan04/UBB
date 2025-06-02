package com.imagevoting.servlet;

import com.imagevoting.dao.ImageDAO;
import com.imagevoting.model.Image;

import javax.servlet.ServletException;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.Part;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.UUID;

@MultipartConfig(
    maxFileSize = 10 * 1024 * 1024,      // 10 MB
    maxRequestSize = 20 * 1024 * 1024,   // 20 MB
    fileSizeThreshold = 1024 * 1024      // 1 MB
)
public class ImageUploadServlet extends HttpServlet {
    
    private ImageDAO imageDAO;
    private static final String UPLOAD_DIR = "uploads";
    private static final String[] ALLOWED_EXTENSIONS = {".jpg", ".jpeg", ".png", ".gif", ".bmp"};
    
    @Override
    public void init() throws ServletException {
        imageDAO = new ImageDAO();
    }
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // Forward to upload page
        request.getRequestDispatcher("/upload.jsp").forward(request, response);
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        HttpSession session = request.getSession();
        Integer userId = (Integer) session.getAttribute("userId");
        
        if (userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }
        
        try {
            // Get form parameters
            String title = request.getParameter("title");
            String description = request.getParameter("description");
            Part filePart = request.getPart("image");
            
            // Validate input
            if (title == null || title.trim().isEmpty()) {
                request.setAttribute("error", "Title is required.");
                request.getRequestDispatcher("/upload.jsp").forward(request, response);
                return;
            }
            
            if (filePart == null || filePart.getSize() == 0) {
                request.setAttribute("error", "Please select an image file.");
                request.getRequestDispatcher("/upload.jsp").forward(request, response);
                return;
            }
            
            // Get original filename
            String originalFilename = Paths.get(filePart.getSubmittedFileName()).getFileName().toString();
            
            // Validate file extension
            if (!isValidImageFile(originalFilename)) {
                request.setAttribute("error", "Invalid file type. Please upload JPG, PNG, GIF, or BMP files only.");
                request.getRequestDispatcher("/upload.jsp").forward(request, response);
                return;
            }
            
            // Generate unique filename
            String fileExtension = getFileExtension(originalFilename);
            String uniqueFilename = UUID.randomUUID().toString() + fileExtension;
            
            // Get upload directory path
            String uploadPath = getServletContext().getRealPath("") + File.separator + UPLOAD_DIR;
            File uploadDir = new File(uploadPath);
            if (!uploadDir.exists()) {
                uploadDir.mkdirs();
            }
            
            // Save file to disk
            String filePath = uploadPath + File.separator + uniqueFilename;
            filePart.write(filePath);
            
            // Create image record in database
            String relativePath = UPLOAD_DIR + "/" + uniqueFilename;
            Image image = new Image(userId, uniqueFilename, originalFilename, title.trim(), 
                                  description != null ? description.trim() : "", relativePath);
            
            if (imageDAO.createImage(image)) {
                // Upload successful
                request.setAttribute("success", "Image uploaded successfully!");
                response.sendRedirect(request.getContextPath() + "/dashboard");
            } else {
                // Database save failed, delete uploaded file
                new File(filePath).delete();
                request.setAttribute("error", "Failed to save image information. Please try again.");
                request.getRequestDispatcher("/upload.jsp").forward(request, response);
            }
            
        } catch (Exception e) {
            System.err.println("Error uploading image: " + e.getMessage());
            e.printStackTrace();
            request.setAttribute("error", "An error occurred while uploading the image. Please try again.");
            request.getRequestDispatcher("/upload.jsp").forward(request, response);
        }
    }
    
    /**
     * Check if the file has a valid image extension
     */
    private boolean isValidImageFile(String filename) {
        if (filename == null || filename.isEmpty()) {
            return false;
        }
        
        String lowerCaseFilename = filename.toLowerCase();
        for (String extension : ALLOWED_EXTENSIONS) {
            if (lowerCaseFilename.endsWith(extension)) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Get file extension from filename
     */
    private String getFileExtension(String filename) {
        if (filename == null || filename.isEmpty()) {
            return "";
        }
        
        int lastDotIndex = filename.lastIndexOf('.');
        if (lastDotIndex > 0 && lastDotIndex < filename.length() - 1) {
            return filename.substring(lastDotIndex);
        }
        return "";
    }
} 