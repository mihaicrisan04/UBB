package com.imagevoting.servlet;

import com.imagevoting.dao.ImageDAO;
import com.imagevoting.model.Image;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.List;

public class XTopImagesServlet extends HttpServlet {
    
    private ImageDAO imageDAO;
    
    @Override
    public void init() throws ServletException {
        imageDAO = new ImageDAO();
    }
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        HttpSession session = request.getSession();
        Integer userId = (Integer) session.getAttribute("userId");
        
        if (userId == null) {
            response.sendRedirect(request.getContextPath() + "/login.jsp");
            return;
        }
        
        // Get the number of top images to display (default to 10)
        String limitStr = request.getParameter("limit");
        int limit = 10; // default value
        
        if (limitStr != null && !limitStr.trim().isEmpty()) {
            try {
                limit = Integer.parseInt(limitStr.trim());
                if (limit <= 0) {
                    limit = 10;
                } else if (limit > 100) {
                    limit = 100; // Maximum limit
                }
            } catch (NumberFormatException e) {
                limit = 10; // Use default if parsing fails
            }
        }
        
        // Get top images
        List<Image> topImages = imageDAO.getTopImages(limit);
        
        // Set attributes for JSP
        request.setAttribute("topImages", topImages);
        request.setAttribute("limit", limit);
        request.setAttribute("currentUserId", userId);
        
        // Forward to top images JSP
        request.getRequestDispatcher("/top-images.jsp").forward(request, response);
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        doGet(request, response);
    }
} 