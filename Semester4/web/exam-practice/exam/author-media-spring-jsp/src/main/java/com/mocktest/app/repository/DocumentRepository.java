package com.mocktest.app.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.mocktest.app.model.Document;
import org.springframework.stereotype.Repository;

@Repository
public interface DocumentRepository extends JpaRepository<Document, Integer> {
}
