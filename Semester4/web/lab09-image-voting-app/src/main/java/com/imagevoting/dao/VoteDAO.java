package com.imagevoting.dao;

import com.imagevoting.model.Vote;
import com.imagevoting.util.DatabaseConnection;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class VoteDAO {
    
    /**
     * Add or update a vote for an image
     * @param vote Vote object to add/update
     * @return true if vote was added/updated successfully, false otherwise
     */
    public boolean addOrUpdateVote(Vote vote) {
        // First check if user has already voted for this image
        String checkSql = "SELECT id FROM votes WHERE user_id = ? AND image_id = ?";
        String insertSql = "INSERT INTO votes (user_id, image_id, vote_value) VALUES (?, ?, ?)";
        String updateSql = "UPDATE votes SET vote_value = ? WHERE user_id = ? AND image_id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection()) {
            
            // Check if vote exists
            try (PreparedStatement checkStatement = connection.prepareStatement(checkSql)) {
                checkStatement.setInt(1, vote.getUserId());
                checkStatement.setInt(2, vote.getImageId());
                
                try (ResultSet resultSet = checkStatement.executeQuery()) {
                    if (resultSet.next()) {
                        // Vote exists, update it
                        try (PreparedStatement updateStatement = connection.prepareStatement(updateSql)) {
                            updateStatement.setInt(1, vote.getVoteValue());
                            updateStatement.setInt(2, vote.getUserId());
                            updateStatement.setInt(3, vote.getImageId());
                            
                            return updateStatement.executeUpdate() > 0;
                        }
                    } else {
                        // Vote doesn't exist, insert new one
                        try (PreparedStatement insertStatement = connection.prepareStatement(insertSql, Statement.RETURN_GENERATED_KEYS)) {
                            insertStatement.setInt(1, vote.getUserId());
                            insertStatement.setInt(2, vote.getImageId());
                            insertStatement.setInt(3, vote.getVoteValue());
                            
                            int rowsAffected = insertStatement.executeUpdate();
                            
                            if (rowsAffected > 0) {
                                // Get the generated ID
                                try (ResultSet generatedKeys = insertStatement.getGeneratedKeys()) {
                                    if (generatedKeys.next()) {
                                        vote.setId(generatedKeys.getInt(1));
                                    }
                                }
                                return true;
                            }
                        }
                    }
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error adding/updating vote: " + e.getMessage());
        }
        
        return false;
    }
    
    /**
     * Get vote by user and image
     * @param userId User ID
     * @param imageId Image ID
     * @return Vote object if found, null otherwise
     */
    public Vote getVoteByUserAndImage(int userId, int imageId) {
        String sql = "SELECT * FROM votes WHERE user_id = ? AND image_id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, userId);
            statement.setInt(2, imageId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    return mapResultSetToVote(resultSet);
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting vote by user and image: " + e.getMessage());
        }
        
        return null;
    }
    
    /**
     * Get all votes for an image
     * @param imageId Image ID
     * @return List of votes for the image
     */
    public List<Vote> getVotesByImageId(int imageId) {
        List<Vote> votes = new ArrayList<>();
        String sql = "SELECT * FROM votes WHERE image_id = ? ORDER BY vote_date DESC";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, imageId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                while (resultSet.next()) {
                    votes.add(mapResultSetToVote(resultSet));
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting votes by image ID: " + e.getMessage());
        }
        
        return votes;
    }
    
    /**
     * Get all votes by a user
     * @param userId User ID
     * @return List of votes by the user
     */
    public List<Vote> getVotesByUserId(int userId) {
        List<Vote> votes = new ArrayList<>();
        String sql = "SELECT * FROM votes WHERE user_id = ? ORDER BY vote_date DESC";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, userId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                while (resultSet.next()) {
                    votes.add(mapResultSetToVote(resultSet));
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting votes by user ID: " + e.getMessage());
        }
        
        return votes;
    }
    
    /**
     * Delete a vote
     * @param userId User ID
     * @param imageId Image ID
     * @return true if vote was deleted successfully, false otherwise
     */
    public boolean deleteVote(int userId, int imageId) {
        String sql = "DELETE FROM votes WHERE user_id = ? AND image_id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, userId);
            statement.setInt(2, imageId);
            
            int rowsAffected = statement.executeUpdate();
            return rowsAffected > 0;
            
        } catch (SQLException e) {
            System.err.println("Error deleting vote: " + e.getMessage());
        }
        
        return false;
    }
    
    /**
     * Get total votes for an image
     * @param imageId Image ID
     * @return Total vote count for the image
     */
    public int getTotalVotesForImage(int imageId) {
        String sql = "SELECT COALESCE(SUM(vote_value), 0) as total_votes FROM votes WHERE image_id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, imageId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    return resultSet.getInt("total_votes");
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting total votes for image: " + e.getMessage());
        }
        
        return 0;
    }
    
    /**
     * Check if user has voted for an image
     * @param userId User ID
     * @param imageId Image ID
     * @return true if user has voted for the image, false otherwise
     */
    public boolean hasUserVotedForImage(int userId, int imageId) {
        String sql = "SELECT COUNT(*) FROM votes WHERE user_id = ? AND image_id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, userId);
            statement.setInt(2, imageId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    return resultSet.getInt(1) > 0;
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error checking if user voted for image: " + e.getMessage());
        }
        
        return false;
    }
    
    /**
     * Get vote statistics for an image
     * @param imageId Image ID
     * @return Array with [total_votes, vote_count, average_vote]
     */
    public int[] getVoteStatistics(int imageId) {
        String sql = "SELECT COUNT(*) as vote_count, COALESCE(SUM(vote_value), 0) as total_votes, " +
                    "COALESCE(AVG(vote_value), 0) as avg_vote FROM votes WHERE image_id = ?";
        
        try (Connection connection = DatabaseConnection.getConnection();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            
            statement.setInt(1, imageId);
            
            try (ResultSet resultSet = statement.executeQuery()) {
                if (resultSet.next()) {
                    return new int[]{
                        resultSet.getInt("total_votes"),
                        resultSet.getInt("vote_count"),
                        (int) Math.round(resultSet.getDouble("avg_vote"))
                    };
                }
            }
            
        } catch (SQLException e) {
            System.err.println("Error getting vote statistics: " + e.getMessage());
        }
        
        return new int[]{0, 0, 0};
    }
    
    /**
     * Map ResultSet to Vote object
     * @param resultSet ResultSet from database query
     * @return Vote object
     * @throws SQLException if error occurs while reading ResultSet
     */
    private Vote mapResultSetToVote(ResultSet resultSet) throws SQLException {
        Vote vote = new Vote();
        vote.setId(resultSet.getInt("id"));
        vote.setUserId(resultSet.getInt("user_id"));
        vote.setImageId(resultSet.getInt("image_id"));
        vote.setVoteValue(resultSet.getInt("vote_value"));
        vote.setVoteDate(resultSet.getTimestamp("vote_date"));
        return vote;
    }
} 