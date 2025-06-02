package com.imagevoting.servlet;

import com.imagevoting.dao.UserDAO;
import com.imagevoting.model.User;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class RegisterServlet extends HttpServlet {
    
    private UserDAO userDAO;
    
    @Override
    public void init() throws ServletException {
        userDAO = new UserDAO();
    }
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        // Forward to register page
        request.getRequestDispatcher("/register.jsp").forward(request, response);
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        
        String username = request.getParameter("username");
        String password = request.getParameter("password");
        String confirmPassword = request.getParameter("confirmPassword");
        String email = request.getParameter("email");
        
        // Validate input
        if (username == null || username.trim().isEmpty()) {
            request.setAttribute("error", "Username is required.");
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        if (password == null || password.trim().isEmpty()) {
            request.setAttribute("error", "Password is required.");
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        if (confirmPassword == null || !password.equals(confirmPassword)) {
            request.setAttribute("error", "Passwords do not match.");
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        if (email == null || email.trim().isEmpty()) {
            request.setAttribute("error", "Email is required.");
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        // Validate username length
        if (username.trim().length() < 3) {
            request.setAttribute("error", "Username must be at least 3 characters long.");
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        // Validate password length
        if (password.length() < 6) {
            request.setAttribute("error", "Password must be at least 6 characters long.");
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        // Check if username already exists
        if (userDAO.usernameExists(username.trim())) {
            request.setAttribute("error", "Username already exists. Please choose a different username.");
            request.setAttribute("username", username);
            request.setAttribute("email", email);
            request.getRequestDispatcher("/register.jsp").forward(request, response);
            return;
        }
        
        // Create new user
        User newUser = new User(username.trim(), password, email.trim());
        
        if (userDAO.createUser(newUser)) {
            // Registration successful
            request.setAttribute("success", "Registration successful! Please login with your credentials.");
            request.getRequestDispatcher("/login.jsp").forward(request, response);
        } else {
            // Registration failed
            request.setAttribute("error", "Registration failed. Please try again.");
            request.setAttribute("username", username);
            request.setAttribute("email", email);
            request.getRequestDispatcher("/register.jsp").forward(request, response);
        }
    }
} 