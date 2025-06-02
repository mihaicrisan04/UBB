package com.imagevoting.dao;

import com.imagevoting.model.Image;
import com.imagevoting.util.DatabaseConnection;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class ImageDAO {
    
    /**
     * Create a new image record
     * @param image Image object to create
     * @return true if image was created successfully, false otherwise
     */
    public boolean createImage(Image image) {
        String sql = "INSERT INTO images (user_id, filename, original_filename, title, description, file_path) VALUES (?, ?, ?, ?, ?, ?)";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)) {
            
            statement.setInt(1, image.getUserId());
            statement.setString(2, image.getFilename());
            statement.setString(3, image.getOriginalFilename());
            statement.setString(4, image.getTitle());
            statement.setString(5, image.getDescription());
            statement.setString(6, image.getFilePath());
            
            int rowsAffected = statement.executeUpdate();
            
            if (rowsAffected > 0) {
                // Get the generated ID
                try (ResultSet generatedKeys = statement.getGeneratedKeys()) {
                    if (generatedKeys.next()) {
                        image.setId(generatedKeys.getInt(1));
                    }
                }
                return true;
            }
            
        } catch (SQLException e) {
            System.err.println("Error creating image: " + e.getMessage());
        }
        
        return false;
    }
    
    /**
     * Get all images with uploader username
     * @return List of all images
     */
    public List<Image> getAllImages() {
        List<Image> images = new ArrayList<>();
        String sql = "SELECT i.*, u.username as uploader_username FROM images i " +
                    "JOIN users u ON i.user_id = u.id " +
                    "ORDER BY i.upload_date DESC";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql);
             ResultSet resultSet = statement.executeQuery()) {
            
            while (resultSet.next()) {
                images.add(mapResultSetToImage(resultSet));
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting all images: " + e.getMessage());
        }
        
        return images;
    }
    
    /**
     * Get images by user ID
     * @param userId User ID
     * @return List of images uploaded by the user
     */
    public List<Image> getImagesByUserId(int userId) {
        List<Image> images = new ArrayList<>();
        String sql = "SELECT i.*, u.username as uploader_username FROM images i " +
                    "JOIN users u ON i.user_id = u.id " +
                    "WHERE i.user_id = ? ORDER BY i.upload_date DESC";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, userId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                while (resultSet.next()) {
                    images.add(mapResultSetToImage(resultSet));
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting images by user ID: " + e.getMessage());
        }
        
        return images;
    }
    
    /**
     * Get image by ID
     * @param imageId Image ID
     * @return Image object if found, null otherwise
     */
    public Image getImageById(int imageId) {
        String sql = "SELECT i.*, u.username as uploader_username FROM images i " +
                    "JOIN users u ON i.user_id = u.id " +
                    "WHERE i.id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, imageId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    return mapResultSetToImage(resultSet);
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting image by ID: " + e.getMessage());
        }
        
        return null;
    }
    
    /**
     * Get top N images by votes
     * @param limit Number of top images to retrieve
     * @return List of top voted images
     */
    public List<Image> getTopImages(int limit) {
        List<Image> images = new ArrayList<>();
        String sql = "SELECT i.*, u.username as uploader_username FROM images i " +
                    "JOIN users u ON i.user_id = u.id " +
                    "ORDER BY i.total_votes DESC, i.upload_date DESC LIMIT ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, limit);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                while (resultSet.next()) {
                    images.add(mapResultSetToImage(resultSet));
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting top images: " + e.getMessage());
        }
        
        return images;
    }
    
    /**
     * Update image information
     * @param image Image object with updated information
     * @return true if update was successful, false otherwise
     */
    public boolean updateImage(Image image) {
        String sql = "UPDATE images SET title = ?, description = ? WHERE id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setString(1, image.getTitle());
            statement.setString(2, image.getDescription());
            statement.setInt(3, image.getId());
            
            int rowsAffected = statement.executeUpdate();
            return rowsAffected > 0;
            
        } catch (SQLException e) {
            System.err.println("Error updating image: " + e.getMessage());
        }
        
        return false;
    }
    
    /**
     * Delete image by ID
     * @param imageId Image ID to delete
     * @return true if deletion was successful, false otherwise
     */
    public boolean deleteImage(int imageId) {
        String sql = "DELETE FROM images WHERE id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, imageId);
            
            int rowsAffected = statement.executeUpdate();
            return rowsAffected > 0;
            
        } catch (SQLException e) {
            System.err.println("Error deleting image: " + e.getMessage());
        }
        
        return false;
    }
    
    /**
     * Get images that user hasn't voted for (excluding user's own images)
     * @param userId User ID
     * @return List of images user can vote for
     */
    public List<Image> getVotableImages(int userId) {
        List<Image> images = new ArrayList<>();
        String sql = "SELECT i.*, u.username as uploader_username FROM images i " +
                    "JOIN users u ON i.user_id = u.id " +
                    "WHERE i.user_id != ? AND i.id NOT IN " +
                    "(SELECT image_id FROM votes WHERE user_id = ?) " +
                    "ORDER BY i.upload_date DESC";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, userId);
            statement.setInt(2, userId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                while (resultSet.next()) {
                    images.add(mapResultSetToImage(resultSet));
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting votable images: " + e.getMessage());
        }
        
        return images;
    }
    
    /**
     * Map ResultSet to Image object
     * @param resultSet ResultSet from database query
     * @return Image object
     * @throws SQLException if error occurs while reading ResultSet
     */
    private Image mapResultSetToImage(ResultSet resultSet) throws SQLException {
        Image image = new Image();
        image.setId(resultSet.getInt("id"));
        image.setUserId(resultSet.getInt("user_id"));
        image.setFilename(resultSet.getString("filename"));
        image.setOriginalFilename(resultSet.getString("original_filename"));
        image.setTitle(resultSet.getString("title"));
        image.setDescription(resultSet.getString("description"));
        image.setFilePath(resultSet.getString("file_path"));
        image.setUploadDate(resultSet.getTimestamp("upload_date"));
        image.setTotalVotes(resultSet.getInt("total_votes"));
        image.setUploaderUsername(resultSet.getString("uploader_username"));
        return image;
    }
} 