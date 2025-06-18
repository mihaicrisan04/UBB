package com.mocktest.app.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.mocktest.app.model.User;

import org.springframework.stereotype.Repository;
import java.util.Optional;

@Repository
public interface UserRepository extends JpaRepository<User, Integer> {
    Optional<User> findByUsername(String username);
}
