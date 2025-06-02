package com.imagevoting.model;

import java.sql.Timestamp;

public class Vote {
    private int id;
    private int userId;
    private int imageId;
    private int voteValue;
    private Timestamp voteDate;
    
    // Default constructor
    public Vote() {}
    
    // Constructor with basic parameters
    public Vote(int userId, int imageId, int voteValue) {
        this.userId = userId;
        this.imageId = imageId;
        this.voteValue = voteValue;
    }
    
    // Full constructor
    public Vote(int id, int userId, int imageId, int voteValue, Timestamp voteDate) {
        this.id = id;
        this.userId = userId;
        this.imageId = imageId;
        this.voteValue = voteValue;
        this.voteDate = voteDate;
    }
    
    // Getters and setters
    public int getId() {
        return id;
    }
    
    public void setId(int id) {
        this.id = id;
    }
    
    public int getUserId() {
        return userId;
    }
    
    public void setUserId(int userId) {
        this.userId = userId;
    }
    
    public int getImageId() {
        return imageId;
    }
    
    public void setImageId(int imageId) {
        this.imageId = imageId;
    }
    
    public int getVoteValue() {
        return voteValue;
    }
    
    public void setVoteValue(int voteValue) {
        this.voteValue = voteValue;
    }
    
    public Timestamp getVoteDate() {
        return voteDate;
    }
    
    public void setVoteDate(Timestamp voteDate) {
        this.voteDate = voteDate;
    }
    
    @Override
    public String toString() {
        return "Vote{" +
                "id=" + id +
                ", userId=" + userId +
                ", imageId=" + imageId +
                ", voteValue=" + voteValue +
                ", voteDate=" + voteDate +
                '}';
    }
} 