import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router, RouterLink } from '@angular/router';
import { ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { AuthService } from '../../auth.service';
import { RegisterDto } from '../../user.service';
import { passwordMatchValidator } from '../../validators/password-match.validator'; // We'll create this

@Component({
  selector: 'app-register',
  standalone: true,
  imports: [CommonModule, ReactiveFormsModule, RouterLink],
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.scss'] // You might want to create a similar SCSS file
})
export class RegisterComponent {
  private fb = inject(FormBuilder);
  private authService = inject(AuthService);
  private router = inject(Router);

  registerForm: FormGroup;
  isLoading = false;
  errorMessage: string | null = null;
  successMessage: string | null = null;

  // Re-use or adjust these as needed from UserAddComponent
  roles: string[] = ['User', 'Editor', 'Admin']; // Default role for new registration might be just 'User'
  genders: string[] = ['Male', 'Female', 'Other'];

  constructor() {
    this.registerForm = this.fb.group({
      name: ['', [Validators.required, Validators.minLength(2)]],
      username: ['', [Validators.required, Validators.minLength(3)]],
      email: ['', [Validators.required, Validators.email]],
      password: ['', [Validators.required, Validators.minLength(6)]], // Add custom validator for strength if needed
      confirmPassword: ['', [Validators.required]],
      age: [null, [Validators.required, Validators.min(18), Validators.max(120)]],
      role: ['User', Validators.required], // Default role to 'User' for new registrations
      gender: ['', Validators.required],
      webpage: ['', [Validators.pattern('^(https?://.+)?$')]],
      profile: ['']
    }, { validators: passwordMatchValidator('password', 'confirmPassword') });

    if (this.authService.currentUserValue) {
        this.router.navigate(['/']); 
    }
  }

  onSubmit(): void {
    if (this.registerForm.invalid) {
      this.errorMessage = 'Please correct the errors in the form.';
      this.registerForm.markAllAsTouched();
      return;
    }

    this.isLoading = true;
    this.errorMessage = null;
    this.successMessage = null;

    // Exclude confirmPassword from the DTO
    const { confirmPassword, ...regDto } = this.registerForm.value;
    const registerDto: RegisterDto = {
        ...regDto,
        age: Number(regDto.age)
    };

    this.authService.register(registerDto).subscribe({
      next: (response) => {
        this.isLoading = false;
        if (response.status === 'Success') {
          this.successMessage = response.message || 'Registration successful! Please login.';
          // Redirect to login page after a short delay
          setTimeout(() => this.router.navigate(['/login']), 2000);
        } else {
          this.errorMessage = response.message || 'Registration failed.';
        }
      },
      error: (err) => {
        this.isLoading = false;
        this.errorMessage = err.message || 'An unexpected error occurred during registration.';
      }
    });
  }

  // Helper getters for template validation
  get name() { return this.registerForm.get('name'); }
  get username() { return this.registerForm.get('username'); }
  get email() { return this.registerForm.get('email'); }
  get password() { return this.registerForm.get('password'); }
  get confirmPassword() { return this.registerForm.get('confirmPassword'); }
  get age() { return this.registerForm.get('age'); }
  get role() { return this.registerForm.get('role'); }
  get gender() { return this.registerForm.get('gender'); }
  get webpage() { return this.registerForm.get('webpage'); }
} 