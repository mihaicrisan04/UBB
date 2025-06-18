package com.mocktest.app.model;
import jakarta.persistence.*;

@Entity
@Table(name = "User")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;
    private String username;

    public int getId() { return id; }
    public void setId(int id) { this.id = id; }
    public String getName() { return username; }
    public void setName(String username) { this.username = username; }
}


