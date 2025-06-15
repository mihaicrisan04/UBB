package com.am.dao;

import com.am.model.Movie;
import com.am.util.DatabaseConnection;
import java.sql.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class MovieDAO {

    public List<Movie> findByIds(List<Integer> ids) throws SQLException {
        if (ids == null || ids.isEmpty()) return Collections.emptyList();
        List<Movie> movies = new ArrayList<>();
        String inSql = String.join(",", Collections.nCopies(ids.size(), "?"));
        String sql = String.format("SELECT * FROM Movies WHERE id IN (%s)", inSql);

        try (Connection conn = DatabaseConnection.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            for (int i = 0; i < ids.size(); i++) {
                ps.setInt(i + 1, ids.get(i));
            }
            ResultSet rs = ps.executeQuery();
            while (rs.next()) {
                Movie movie = new Movie();
                movie.setId(rs.getInt("id"));
                movie.setTitle(rs.getString("title"));
                movie.setDuration(rs.getInt("duration"));
                movies.add(movie);
            }
        }
        return movies;
    }
}