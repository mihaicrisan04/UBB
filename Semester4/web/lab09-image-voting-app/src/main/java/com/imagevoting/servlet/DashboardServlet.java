package com.imagevoting.servlet;

import com.imagevoting.dao.ImageDAO;
import com.imagevoting.dao.VoteDAO;
import com.imagevoting.model.Image;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.List;

public class DashboardServlet extends HttpServlet {
    
    private ImageDAO imageDAO;
    private VoteDAO voteDAO;
    
    @Override
    public void init() throws ServletException {
        imageDAO = new ImageDAO();
        voteDAO = new VoteDAO();
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
        
        // Get all images
        List<Image> allImages = imageDAO.getAllImages();
        
        // Get images that user can vote for (not their own and not already voted)
        List<Image> votableImages = imageDAO.getVotableImages(userId);
        
        // Get user's own images
        List<Image> userImages = imageDAO.getImagesByUserId(userId);
        
        // Set attributes for JSP
        request.setAttribute("allImages", allImages);
        request.setAttribute("votableImages", votableImages);
        request.setAttribute("userImages", userImages);
        request.setAttribute("currentUserId", userId);
        
        // Forward to dashboard JSP
        request.getRequestDispatcher("/dashboard.jsp").forward(request, response);
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        doGet(request, response);
    }
} 