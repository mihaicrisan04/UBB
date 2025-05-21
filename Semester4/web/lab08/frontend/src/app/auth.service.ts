import { Injectable, inject } from '@angular/core';
import { BehaviorSubject, Observable, tap } from 'rxjs';
import { Router } from '@angular/router';
import { UserService, User, LoginDto, RegisterDto } from './user.service'; // Assuming DotNetApiResponse is also exported or handled

interface AuthResponse {
  status?: string;
  message?: string;
  user?: User; // Backend might return user details on login/register
  userId?: string;
}

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private userService = inject(UserService);
  private router = inject(Router);

  // BehaviorSubject to hold the current user state. null if not logged in.
  // We make it private and expose an observable for components to subscribe to.
  private currentUserSubject = new BehaviorSubject<User | null>(this.getInitialUser());
  public currentUser$ = this.currentUserSubject.asObservable();
  public isAuthenticated$ = new BehaviorSubject<boolean>(!!this.getInitialUser());

  constructor() {
    // When the service initializes, check if there's a stored user
    // This handles page refreshes
    const storedUser = this.getInitialUser();
    if (storedUser) {
      this.currentUserSubject.next(storedUser);
      this.isAuthenticated$.next(true);
    }
  }

  private getInitialUser(): User | null {
    const userJson = localStorage.getItem('currentUser');
    return userJson ? JSON.parse(userJson) : null;
  }

  public get currentUserValue(): User | null {
    return this.currentUserSubject.value;
  }

  login(loginDto: LoginDto): Observable<AuthResponse> {
    return this.userService.loginUser(loginDto).pipe(
      tap(response => {
        console.log('Login response:', response);
        console.log('Login response.status:', response.status);
        console.log('Login response.user:', response.user);
        if (response.status === 'Success' && response.user) {
          localStorage.setItem('currentUser', JSON.stringify(response.user));
          this.currentUserSubject.next(response.user);
          this.isAuthenticated$.next(true);
          // Navigate to a default route after login, e.g., '/' or '/dashboard'
          // this.router.navigate(['/']); 
        } else if (response.status === 'Success') {
          // If login is successful but user object isn't returned directly in login response,
          // you might need another call to fetch user details or handle it differently.
          // For now, assume login response includes user or we navigate and let other components fetch if needed.
          console.warn('Login successful, but no user object in response. Auth state updated minimally.');
          // If your backend sets an HTTPOnly cookie and doesn't return user, 
          // you may need a separate /api/account/me endpoint to get current user
          this.isAuthenticated$.next(true); // Assume logged in based on success
        }
      })
      // catchError is handled by UserService, but you can add specific login error handling here if needed
    );
  }

  register(registerDto: RegisterDto): Observable<AuthResponse> {
    return this.userService.registerUser(registerDto).pipe(
      tap(response => {
        // Typically after registration, the user is NOT automatically logged in.
        // They are redirected to the login page.
        // if (response.status === 'Success') {
        //   // Optionally log them in directly or fetch user data
        // }
      })
    );
  }

  logout(): void {
    this.userService.logoutUser().subscribe({
      next: () => {
        localStorage.removeItem('currentUser');
        this.currentUserSubject.next(null);
        this.isAuthenticated$.next(false);
        this.router.navigate(['/login']);
      },
      error: (err) => {
        console.error('Logout failed', err);
        // Still clear local state on error as a precaution, or handle differently
        localStorage.removeItem('currentUser');
        this.currentUserSubject.next(null);
        this.isAuthenticated$.next(false);
        this.router.navigate(['/login']);
      }
    });
  }
} 