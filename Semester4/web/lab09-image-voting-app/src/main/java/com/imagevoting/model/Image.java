package com.imagevoting.model;

import java.sql.Timestamp;

public class Image {
    private int id;
    private int userId;
    private String filename;
    private String originalFilename;
    private String title;
    private String description;
    private String filePath;
    private Timestamp uploadDate;
    private int totalVotes;
    private String uploaderUsername; // For display purposes
    
    // Default constructor
    public Image() {}
    
    // Constructor with basic parameters
    public Image(int userId, String filename, String originalFilename, String title, String description, String filePath) {
        this.userId = userId;
        this.filename = filename;
        this.originalFilename = originalFilename;
        this.title = title;
        this.description = description;
        this.filePath = filePath;
    }
    
    // Full constructor
    public Image(int id, int userId, String filename, String originalFilename, String title, 
                String description, String filePath, Timestamp uploadDate, int totalVotes) {
        this.id = id;
        this.userId = userId;
        this.filename = filename;
        this.originalFilename = originalFilename;
        this.title = title;
        this.description = description;
        this.filePath = filePath;
        this.uploadDate = uploadDate;
        this.totalVotes = totalVotes;
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
    
    public String getFilename() {
        return filename;
    }
    
    public void setFilename(String filename) {
        this.filename = filename;
    }
    
    public String getOriginalFilename() {
        return originalFilename;
    }
    
    public void setOriginalFilename(String originalFilename) {
        this.originalFilename = originalFilename;
    }
    
    public String getTitle() {
        return title;
    }
    
    public void setTitle(String title) {
        this.title = title;
    }
    
    public String getDescription() {
        return description;
    }
    
    public void setDescription(String description) {
        this.description = description;
    }
    
    public String getFilePath() {
        return filePath;
    }
    
    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }
    
    public Timestamp getUploadDate() {
        return uploadDate;
    }
    
    public void setUploadDate(Timestamp uploadDate) {
        this.uploadDate = uploadDate;
    }
    
    public int getTotalVotes() {
        return totalVotes;
    }
    
    public void setTotalVotes(int totalVotes) {
        this.totalVotes = totalVotes;
    }
    
    public String getUploaderUsername() {
        return uploaderUsername;
    }
    
    public void setUploaderUsername(String uploaderUsername) {
        this.uploaderUsername = uploaderUsername;
    }
    
    @Override
    public String toString() {
        return "Image{" +
                "id=" + id +
                ", userId=" + userId +
                ", filename='" + filename + '\'' +
                ", title='" + title + '\'' +
                ", totalVotes=" + totalVotes +
                ", uploadDate=" + uploadDate +
                '}';
    }
} 