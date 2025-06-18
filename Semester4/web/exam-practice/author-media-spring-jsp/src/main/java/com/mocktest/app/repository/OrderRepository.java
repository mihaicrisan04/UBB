package com.mocktest.app.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import com.mocktest.app.model.Order;
import org.springframework.stereotype.Repository;
import java.util.List;

@Repository
public interface OrderRepository extends JpaRepository<Order, Integer> {
    List<Order> findByUserIdOrderByIdDesc(int userId);
}
