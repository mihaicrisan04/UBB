package com.am.controller;

import com.am.dao.*;
import com.am.model.*;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.*;
import java.io.IOException;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;

@WebServlet("/app")
public class MainServlet extends HttpServlet {
    private AuthorDAO authorDAO;
    private DocumentDAO documentDAO;
    private MovieDAO movieDAO;

    @Override
    public void init() {
        authorDAO = new AuthorDAO();
        documentDAO = new DocumentDAO();
        movieDAO = new MovieDAO();
    }

    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String action = req.getParameter("action");
        try {
            if ("login".equals(action)) {
                login(req, resp);
            } else if ("addDocument".equals(action)) {
                addDocument(req, resp);
            }
        } catch (SQLException e) {
            throw new ServletException("Database error", e);
        }
    }

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        String action = req.getParameter("action");
        try {
            if ("logout".equals(action)) {
                logout(req, resp);
            } else if ("deleteMovie".equals(action)) {
                deleteMovie(req, resp);
            } else {
                showDashboard(req, resp);
            }
        } catch (SQLException e) {
            throw new ServletException("Database error", e);
        }
    }

    private void login(HttpServletRequest req, HttpServletResponse resp) throws SQLException, IOException, ServletException {
        String name = req.getParameter("name");
        Author author = authorDAO.findByName(name);
        if (author != null) {
            req.getSession().setAttribute("author", author);
            resp.sendRedirect("app");
        } else {
            req.setAttribute("error", "Author not found.");
            req.getRequestDispatcher("/index.jsp").forward(req, resp);
        }
    }



    private void addDocument(HttpServletRequest req, HttpServletResponse resp) throws SQLException, IOException {
        HttpSession session = req.getSession();
        Author author = (Author) session.getAttribute("author");

        String docName = req.getParameter("docName");
        String docContents = req.getParameter("docContents");

        Document newDoc = new Document();
        newDoc.setName(docName);
        newDoc.setContents(docContents);
        int newDocId = documentDAO.add(newDoc);

        String currentDocList = author.getDocumentList();
        String newDocList = (currentDocList == null || currentDocList.isEmpty())
            ? String.valueOf(newDocId)
            : currentDocList + "," + newDocId;
        author.setDocumentList(newDocList);

        authorDAO.update(author);
        session.setAttribute("author", author); // Update session object
        resp.sendRedirect("app");
    }

    private void deleteMovie(HttpServletRequest req, HttpServletResponse resp) throws SQLException, IOException {
        HttpSession session = req.getSession();
        Author author = (Author) session.getAttribute("author");
        int movieIdToDelete = Integer.parseInt(req.getParameter("id"));

        String currentMovieList = author.getMovieList();
        if (currentMovieList != null && !currentMovieList.isEmpty()) {
            List<String> movieIds = new ArrayList<>(Arrays.asList(currentMovieList.split(",")));
            movieIds.remove(String.valueOf(movieIdToDelete));
            String newMovieList = String.join(",", movieIds);
            author.setMovieList(newMovieList);

            authorDAO.update(author);
            session.setAttribute("author", author); // Update session object
        }
        resp.sendRedirect("app");
    }

    private void showDashboard(HttpServletRequest req, HttpServletResponse resp) throws SQLException, ServletException, IOException {
        HttpSession session = req.getSession();
        Author author = (Author) session.getAttribute("author");

        if (author == null) {
            resp.sendRedirect("index.jsp");
            return;
        }

        // Fetch documents
        String docListStr = author.getDocumentList();
        List<Document> documents = new ArrayList<>();
        if (docListStr != null && !docListStr.isEmpty()) {
            List<Integer> docIds = Arrays.stream(docListStr.split(","))
                                         .map(Integer::parseInt)
                                         .collect(Collectors.toList());
            documents = documentDAO.findByIds(docIds);
        }

        // Fetch movies
        String movieListStr = author.getMovieList();
        List<Movie> movies = new ArrayList<>();
        if (movieListStr != null && !movieListStr.isEmpty()) {
            List<Integer> movieIds = Arrays.stream(movieListStr.split(","))
                                           .map(Integer::parseInt)
                                           .collect(Collectors.toList());
            movies = movieDAO.findByIds(movieIds);
        }

        // Fetch document with most authors
        Document docWithMostAuthors = documentDAO.findDocumentWithMostAuthors();

        req.setAttribute("documents", documents);
        req.setAttribute("movies", movies);
        req.setAttribute("docWithMostAuthors", docWithMostAuthors);
        req.getRequestDispatcher("/dashboard.jsp").forward(req, resp);
    }

    private void logout(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        req.getSession().invalidate();
        resp.sendRedirect("index.jsp");
    }
}