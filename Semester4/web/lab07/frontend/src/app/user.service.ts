import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams, HttpErrorResponse } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError, map } from 'rxjs/operators';

// Define the User interface based on the backend structure
export interface User {
  id: number;
  name: string;
  username: string;
  age: number;
  role: string;
  gender: string;
  email: string;
  webpage?: string | null; // Optional fields
  profile?: string | null; // Optional fields
  created_at?: string; // Optional, depends if needed in UI
  password?: string; // Add password for add/edit forms, but it won't be fetched from backend
}

// Define expected API response structures
interface ApiResponse<T> {
  status: 'success' | 'error';
  message?: string;
  users?: T[]; // For get_users, search_users
  user?: T;    // For get_user
  userId?: number; // For add_user
}

@Injectable({
  providedIn: 'root'
})
export class UserService {
  private http = inject(HttpClient);
  // Adjust the base URL to where your PHP API is running
  private apiUrl = 'http://localhost:8000/api'; 

  constructor() { }

  // Example: Get Users
  getUsers(role?: string): Observable<User[]> {
    let params = new HttpParams();
    if (role) {
      params = params.set('role', role);
    }
    return this.http.get<ApiResponse<User>>(`${this.apiUrl}/get_users.php`, { params }).pipe(
      map(response => {
        if (response.status === 'success' && response.users) {
          return response.users;
        } else {
          throw new Error(response.message || 'Failed to fetch users');
        }
      }),
      catchError(this.handleError)
    );
  }

  // Get Single User by ID
  getUser(id: number): Observable<User> {
    return this.http.get<ApiResponse<User>>(`${this.apiUrl}/get_user.php`, { params: { id: id.toString() } }).pipe(
      map(response => {
        if (response.status === 'success' && response.user) {
          return response.user;
        } else {
          throw new Error(response.message || `Failed to fetch user with id ${id}`);
        }
      }),
      catchError(this.handleError)
    );
  }

  // Delete User by ID
  deleteUser(id: number): Observable<{ status: string; message: string }> {
    // Send ID as query parameter for DELETE requests
    return this.http.delete<ApiResponse<never>>(`${this.apiUrl}/delete_user.php`, { params: { id: id.toString() } }).pipe(
      map(response => {
        // The backend might return status/message directly or wrapped in ApiResponse
        // Adjust mapping based on actual backend response structure for DELETE
        if (response && response.status === 'success') {
           return { status: 'success', message: response.message || 'User deleted successfully' };
        } else {
           // Even if status isn't 'success', the backend might send a useful message
           throw new Error(response?.message || 'Failed to delete user');
        }
      }),
      catchError(this.handleError)
    );
  }

  // Add User
  addUser(user: Omit<User, 'id' | 'created_at'>): Observable<ApiResponse<User>> {
    // Backend expects password, frontend form provides it
    return this.http.post<ApiResponse<User>>(`${this.apiUrl}/add_user.php`, user).pipe(
       map(response => {
         // Ensure the response conforms to ApiResponse<User>
         if (response.status === 'success') {
           return response;
         } else {
           throw new Error(response.message || 'Failed to add user');
         }
       }),
      catchError(this.handleError)
    );
  }

  // Update User
  updateUser(user: User): Observable<ApiResponse<User>> {
    // Ensure password is not sent in update unless specifically handled
    const userData: Partial<User> = { ...user };
    delete userData.password; // Remove password unless update logic is added

    return this.http.put<ApiResponse<User>>(`${this.apiUrl}/update_user.php`, userData).pipe(
       map(response => {
         if (response.status === 'success') {
           return response;
         } else {
           throw new Error(response.message || 'Failed to update user');
         }
       }),
      catchError(this.handleError)
    );
  }

  // Search Users
  searchUsers(name: string): Observable<User[]> {
    let params = new HttpParams();
    if (name) {
      params = params.set('name', name);
    }
    return this.http.get<ApiResponse<User>>(`${this.apiUrl}/search_users.php`, { params }).pipe(
      map(response => {
        if (response.status === 'success' && response.users) {
          return response.users;
        } else {
          throw new Error(response.message || 'Failed to search users');
        }
      }),
      catchError(this.handleError)
    );
  }

  // Basic error handler
  private handleError(error: HttpErrorResponse) {
    console.error('API Error:', error);
    let errorMessage = 'An unknown error occurred!';
    if (error.error instanceof ErrorEvent) {
      // Client-side error
      errorMessage = `Error: ${error.error.message}`;
    } else {
      // Server-side error
      errorMessage = `Error Code: ${error.status}\nMessage: ${error.message}`;
      if (error.error && typeof error.error === 'object' && error.error.message) {
         errorMessage += `\nServer Message: ${error.error.message}`;
      }
    }
    return throwError(() => new Error(errorMessage));
  }
}
