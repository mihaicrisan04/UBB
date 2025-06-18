package com.mocktest.app.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.mocktest.app.model.Order;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderRepository extends JpaRepository<Order, Integer> {
}
