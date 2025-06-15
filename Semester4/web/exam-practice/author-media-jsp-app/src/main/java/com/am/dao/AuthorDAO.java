package com.am.dao;

import com.am.model.Author;
import com.am.util.DatabaseConnection;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class AuthorDAO {

    public Author findByName(String name) throws SQLException {
        String sql = "SELECT * FROM Authors WHERE name = ?";
        try (Connection conn = DatabaseConnection.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            ps.setString(1, name);
            ResultSet rs = ps.executeQuery();
            if (rs.next()) {
                Author author = new Author();
                author.setId(rs.getInt("id"));
                author.setName(rs.getString("name"));
                author.setDocumentList(rs.getString("documentList"));
                author.setMovieList(rs.getString("movieList"));
                return author;
            }
        }
        return null;
    }

    public List<Author> findAll() throws SQLException {
        List<Author> authors = new ArrayList<>();
        String sql = "SELECT * FROM Authors";
        try (Connection conn = DatabaseConnection.getConnection();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {
            while (rs.next()) {
                Author author = new Author();
                author.setId(rs.getInt("id"));
                author.setName(rs.getString("name"));
                author.setDocumentList(rs.getString("documentList"));
                author.setMovieList(rs.getString("movieList"));
                authors.add(author);
            }
        }
        return authors;
    }

    public void update(Author author) throws SQLException {
        String sql = "UPDATE Authors SET documentList = ?, movieList = ? WHERE id = ?";
        try (Connection conn = DatabaseConnection.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            ps.setString(1, author.getDocumentList());
            ps.setString(2, author.getMovieList());
            ps.setInt(3, author.getId());
            ps.executeUpdate();
        }
    }
}