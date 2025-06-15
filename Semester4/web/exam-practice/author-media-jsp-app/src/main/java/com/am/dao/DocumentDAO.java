package com.am.dao;

import com.am.model.Author;
import com.am.model.Document;
import com.am.util.DatabaseConnection;
import java.sql.*;
import java.util.*;
import java.util.stream.Collectors;

public class DocumentDAO {

    public int add(Document document) throws SQLException {
        String sql = "INSERT INTO Documents (name, contents) VALUES (?, ?)";
        try (Connection conn = DatabaseConnection.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {
            ps.setString(1, document.getName());
            ps.setString(2, document.getContents());
            ps.executeUpdate();
            ResultSet rs = ps.getGeneratedKeys();
            if (rs.next()) {
                return rs.getInt(1);
            }
        }
        return -1;
    }

    public List<Document> findByIds(List<Integer> ids) throws SQLException {
        if (ids == null || ids.isEmpty()) return Collections.emptyList();
        List<Document> documents = new ArrayList<>();
        String inSql = String.join(",", Collections.nCopies(ids.size(), "?"));
        String sql = String.format("SELECT * FROM Documents WHERE id IN (%s)", inSql);

        try (Connection conn = DatabaseConnection.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {
            for (int i = 0; i < ids.size(); i++) {
                ps.setInt(i + 1, ids.get(i));
            }
            ResultSet rs = ps.executeQuery();
            while (rs.next()) {
                Document doc = new Document();
                doc.setId(rs.getInt("id"));
                doc.setName(rs.getString("name"));
                doc.setContents(rs.getString("contents"));
                documents.add(doc);
            }
        }
        return documents;
    }

    public Document findDocumentWithMostAuthors() throws SQLException {
        AuthorDAO authorDAO = new AuthorDAO();
        List<Author> allAuthors = authorDAO.findAll();
        Map<Integer, Integer> docIdToAuthorCount = new HashMap<>();

        for (Author author : allAuthors) {
            String docListStr = author.getDocumentList();
            if (docListStr != null && !docListStr.isEmpty()) {
                List<Integer> docIds = Arrays.stream(docListStr.split(","))
                                             .map(Integer::parseInt)
                                             .collect(Collectors.toList());
                for (Integer docId : docIds) {
                    docIdToAuthorCount.put(docId, docIdToAuthorCount.getOrDefault(docId, 0) + 1);
                }
            }
        }

        if (docIdToAuthorCount.isEmpty()) return null;

        int maxDocId = Collections.max(docIdToAuthorCount.entrySet(), Map.Entry.comparingByValue()).getKey();
        List<Document> result = findByIds(Collections.singletonList(maxDocId));
        return result.isEmpty() ? null : result.get(0);
    }
}