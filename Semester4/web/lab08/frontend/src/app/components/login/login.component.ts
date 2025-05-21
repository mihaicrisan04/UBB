import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router, RouterLink } from '@angular/router';
import { ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { AuthService } from '../../auth.service';
import { LoginDto } from '../../user.service';

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [CommonModule, ReactiveFormsModule, RouterLink],
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent {
  private fb = inject(FormBuilder);
  private authService = inject(AuthService);
  private router = inject(Router);

  loginForm: FormGroup;
  isLoading = false;
  errorMessage: string | null = null;

  constructor() {
    this.loginForm = this.fb.group({
      username: ['', [Validators.required]],
      password: ['', [Validators.required]],
      rememberMe: [false] // Optional
    });

    // If already logged in, redirect away from login page
    if (this.authService.currentUserValue) {
        this.router.navigate(['/']); // Or to a dashboard route
    }
  }

  onSubmit(): void {
    if (this.loginForm.invalid) {
      this.errorMessage = 'Please enter username and password.';
      this.loginForm.markAllAsTouched();
      return;
    }

    this.isLoading = true;
    this.errorMessage = null;

    const loginDto: LoginDto = this.loginForm.value;

    this.authService.login(loginDto).subscribe({
      next: (response) => {
        this.isLoading = false;
        if (response.status === 'Success') {
          // Navigation is handled by AuthService if login is successful and user data is set
          // Or, if backend only sets a cookie and doesn't return user, 
          // authService updates isAuthenticated$. Then we might navigate here.
          this.router.navigate(['/']); // Navigate to home/dashboard
        } else {
          // This case should ideally be caught by the error block if HTTP status is not 2xx
          // Or if backend returns 200 OK with a custom error status property
          this.errorMessage = response.message || 'Login failed. Please check your credentials.';
        }
      },
      error: (err) => {
        this.isLoading = false;
        this.errorMessage = err.message || 'An unexpected error occurred during login.';
      }
    });
  }

  get username() { return this.loginForm.get('username'); }
  get password() { return this.loginForm.get('password'); }
} 