package com.mocktest.app.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.mocktest.app.model.Movie;
import org.springframework.stereotype.Repository;

@Repository
public interface MovieRepository extends JpaRepository<Movie, Integer> {
}
