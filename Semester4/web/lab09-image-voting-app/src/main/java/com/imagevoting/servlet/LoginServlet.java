package com.imagevoting.servlet;

import com.imagevoting.dao.UserDAO;
import com.imagevoting.model.User;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;

public class LoginServlet extends HttpServlet {
    
    private UserDAO userDAO;
    
    @Override
    public void init() throws ServletException {
        userDAO = new UserDAO();
    }
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // Forward to login page
        request.getRequestDispatcher("/login.jsp").forward(request, response);
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        String username = request.getParameter("username");
        String password = request.getParameter("password");
        
        // Validate input
        if (username == null || username.trim().isEmpty() || 
            password == null || password.trim().isEmpty()) {
            request.setAttribute("error", "Username and password are required.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
            return;
        }
        
        // Authenticate user
        User user = userDAO.authenticateUser(username.trim(), password);
        
        if (user != null) {
            // Authentication successful
            HttpSession session = request.getSession();
            session.setAttribute("user", user);
            session.setAttribute("userId", user.getId());
            session.setAttribute("username", user.getUsername());
            
            // Redirect to dashboard
            response.sendRedirect(request.getContextPath() + "/dashboard");
        } else {
            // Authentication failed
            request.setAttribute("error", "Invalid username or password.");
            request.setAttribute("username", username); // Keep username in form
            request.getRequestDispatcher("/login.jsp").forward(request, response);
        }
    }
} 