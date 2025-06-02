package com.imagevoting.servlet;

import com.imagevoting.dao.VoteDAO;
import com.imagevoting.dao.ImageDAO;
import com.imagevoting.model.Vote;
import com.imagevoting.model.Image;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.io.PrintWriter;

public class VoteServlet extends HttpServlet {
    
    private VoteDAO voteDAO;
    private ImageDAO imageDAO;
    
    @Override
    public void init() throws ServletException {
        voteDAO = new VoteDAO();
        imageDAO = new ImageDAO();
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        HttpSession session = request.getSession();
        Integer userId = (Integer) session.getAttribute("userId");
        
        if (userId == null) {
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.getWriter().write("{\"success\": false, \"message\": \"User not logged in\"}");
            return;
        }
        
        try {
            // Get parameters
            String imageIdStr = request.getParameter("imageId");
            String voteValueStr = request.getParameter("voteValue");
            
            if (imageIdStr == null || voteValueStr == null) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.getWriter().write("{\"success\": false, \"message\": \"Missing parameters\"}");
                return;
            }
            
            int imageId = Integer.parseInt(imageIdStr);
            int voteValue = Integer.parseInt(voteValueStr);
            
            // Validate vote value (must be positive)
            if (voteValue <= 0) {
                response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
                response.getWriter().write("{\"success\": false, \"message\": \"Vote value must be positive\"}");
                return;
            }
            
            // Check if image exists
            Image image = imageDAO.getImageById(imageId);
            if (image == null) {
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                response.getWriter().write("{\"success\": false, \"message\": \"Image not found\"}");
                return;
            }
            
            // Check if user is trying to vote for their own image
            if (image.getUserId() == userId) {
                response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                response.getWriter().write("{\"success\": false, \"message\": \"Cannot vote for your own image\"}");
                return;
            }
            
            // Create or update vote
            Vote vote = new Vote(userId, imageId, voteValue);
            boolean success = voteDAO.addOrUpdateVote(vote);
            
            if (success) {
                // Get updated vote statistics
                int[] stats = voteDAO.getVoteStatistics(imageId);
                int totalVotes = stats[0];
                int voteCount = stats[1];
                int avgVote = stats[2];
                
                // Return JSON response
                response.setContentType("application/json");
                PrintWriter out = response.getWriter();
                out.write("{\"success\": true, \"message\": \"Vote submitted successfully\", " +
                         "\"totalVotes\": " + totalVotes + ", \"voteCount\": " + voteCount + 
                         ", \"avgVote\": " + avgVote + "}");
            } else {
                response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                response.getWriter().write("{\"success\": false, \"message\": \"Failed to submit vote\"}");
            }
            
        } catch (NumberFormatException e) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().write("{\"success\": false, \"message\": \"Invalid number format\"}");
        } catch (Exception e) {
            System.err.println("Error processing vote: " + e.getMessage());
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().write("{\"success\": false, \"message\": \"Internal server error\"}");
        }
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
        
        // Get image ID parameter
        String imageIdStr = request.getParameter("imageId");
        
        if (imageIdStr != null) {
            try {
                int imageId = Integer.parseInt(imageIdStr);
                
                // Get existing vote if any
                Vote existingVote = voteDAO.getVoteByUserAndImage(userId, imageId);
                
                // Get image details
                Image image = imageDAO.getImageById(imageId);
                
                if (image != null) {
                    request.setAttribute("image", image);
                    request.setAttribute("existingVote", existingVote);
                    request.getRequestDispatcher("/vote.jsp").forward(request, response);
                    return;
                }
            } catch (NumberFormatException e) {
                // Invalid image ID
            }
        }
        
        // Redirect to dashboard if no valid image ID
        response.sendRedirect(request.getContextPath() + "/dashboard");
    }
} 