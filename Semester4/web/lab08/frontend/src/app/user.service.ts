import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams, HttpErrorResponse } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError, map } from 'rxjs/operators';

// Updated User interface to match ASP.NET Core UserDto
export interface User {
  id: string; // Changed from number to string
  name: string;
  username: string;
  email: string;
  age: number;
  role: string;
  gender: string;
  webpage?: string | null;
  profile?: string | null;
  // password is used for forms, not stored/fetched directly in User object from backend
  password?: string; 
}

// DTO for registration, matches backend RegisterDto
export interface RegisterDto {
  name: string;
  email: string;
  username: string;
  password?: string;
  age: number;
  role: string;
  gender: string;
  webpage?: string | null;
  profile?: string | null;
}

// DTO for login, matches backend LoginDto
export interface LoginDto {
  username: string;
  password?: string;
  rememberMe?: boolean;
}

// DTO for user update, matches backend UpdateUserDto
export interface UpdateUserDto {
  name?: string;
  age?: number;
  role?: string;
  gender?: string;
  webpage?: string | null;
  profile?: string | null;
  // Email and Username are not typically updated this way with Identity
}

// More specific API response structures from the .NET backend
interface DotNetApiResponse {
  status?: string; // e.g., "Success", "Error"
  message?: string;
  // For login, user details might be nested or returned alongside a token/cookie
  user?: User; 
  userId?: string; // For registration
}

@Injectable({
  providedIn: 'root'
})
export class UserService {
  private http = inject(HttpClient);
  // Adjust if your .NET backend runs on a different port or base path
  private apiUrl = 'http://localhost:5213/api'; 

  constructor() { }

  // Get All Users
  getUsers(role?: string): Observable<User[]> {
    let params = new HttpParams();
    if (role) {
      // Assuming the .NET API supports filtering by role on the /users endpoint
      // If not, this parameter might need to be handled differently or removed
      params = params.set('role', role); 
    }
    // .NET API returns the array directly
    return this.http.get<User[]>(`${this.apiUrl}/users`, { params, withCredentials: true }).pipe(
      catchError(this.handleError)
    );
  }

  // Get Single User by ID
  getUser(id: string): Observable<User> {
    // .NET API returns the user object directly
    return this.http.get<User>(`${this.apiUrl}/users/${id}`, { withCredentials: true }).pipe(
      catchError(this.handleError)
    );
  }

  // Delete User by ID
  deleteUser(id: string): Observable<DotNetApiResponse> {
    return this.http.delete<DotNetApiResponse>(`${this.apiUrl}/users/${id}`, { withCredentials: true }).pipe(
      catchError(this.handleError)
    );
  }

  // Register User (formerly addUser)
  registerUser(userDto: RegisterDto): Observable<DotNetApiResponse> {
    return this.http.post<DotNetApiResponse>(`${this.apiUrl}/account/register`, userDto, { withCredentials: true }).pipe(
      catchError(this.handleError)
    );
  }

  // Login User
  loginUser(loginDto: LoginDto): Observable<DotNetApiResponse> {
    // ASP.NET Core Identity handles cookies automatically.
    // The response might include user details or just a success status.
    return this.http.post<DotNetApiResponse>(`${this.apiUrl}/account/login`, loginDto, { withCredentials: true }).pipe(
      map(response => {
        // Optionally, store user info or token from response in local storage/state management
        // if (response.status === 'Success' && response.user) {
        //   // example: localStorage.setItem('currentUser', JSON.stringify(response.user));
        // }
        return response;
      }),
      catchError(this.handleError)
    );
  }

  // Logout User
  logoutUser(): Observable<DotNetApiResponse> {
    return this.http.post<DotNetApiResponse>(`${this.apiUrl}/account/logout`, {}, { withCredentials: true }).pipe(
      map(response => {
        // Optionally, clear any stored user info
        // example: localStorage.removeItem('currentUser');
        return response;
      }),
      catchError(this.handleError)
    );
  }

  // Update User
  updateUser(id: string, userDto: UpdateUserDto): Observable<DotNetApiResponse> {
    return this.http.put<DotNetApiResponse>(`${this.apiUrl}/users/${id}`, userDto, { withCredentials: true }).pipe(
      catchError(this.handleError)
    );
  }

  // Search Users
  searchUsers(query: string): Observable<User[]> {
    let params = new HttpParams();
    if (query) {
      params = params.set('query', query);
    }
    // .NET API returns the array directly
    return this.http.get<User[]>(`${this.apiUrl}/users/search`, { params, withCredentials: true }).pipe(
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
      // The .NET backend might return structured errors, e.g., in `error.error.errors` or `error.error.title` for ProblemDetails
      errorMessage = `Error Code: ${error.status}\nMessage: ${error.message}`;
      if (error.error) {
        if (typeof error.error === 'string') {
          errorMessage += `\nServer Message: ${error.error}`;
        } else if (error.error.message) {
           errorMessage += `\nServer Message: ${error.error.message}`;
        } else if (error.error.title) { // For ProblemDetails
           errorMessage += `\nServer Title: ${error.error.title}`;
        } else if (error.error.errors) { // For validation errors
           const errors = Object.values(error.error.errors).flat();
           errorMessage += `\nServer Errors: ${errors.join(', ')}`;
        }
      }
    }
    return throwError(() => new Error(errorMessage));
  }
}
