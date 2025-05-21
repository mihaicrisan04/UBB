import { Component, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
// Import ReactiveFormsModule for reactive forms
import { ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { UserService, RegisterDto } from '../../user.service';

@Component({
  selector: 'app-user-add',
  standalone: true,
  // Add ReactiveFormsModule to imports
  imports: [CommonModule, ReactiveFormsModule], 
  templateUrl: './user-add.component.html',
  styleUrls: ['./user-add.component.scss']
})
export class UserAddComponent {
  private fb = inject(FormBuilder);
  private userService = inject(UserService);
  private router = inject(Router);

  userForm: FormGroup;
  isLoading = false;
  errorMessage: string | null = null;
  successMessage: string | null = null;

  roles: string[] = ['Admin', 'User', 'Editor'];
  genders: string[] = ['Male', 'Female', 'Other'];

  constructor() {
    this.userForm = this.fb.group({
      name: ['', [Validators.required, Validators.minLength(2)]],
      username: ['', [Validators.required, Validators.minLength(3)]],
      password: ['', [Validators.required, Validators.minLength(6)]],
      age: [null, [Validators.required, Validators.min(1), Validators.max(120)]],
      role: ['', Validators.required],
      gender: ['', Validators.required],
      email: ['', [Validators.required, Validators.email]],
      webpage: ['', [Validators.pattern('^(https?://.+)?$')]],
      profile: ['']
    });
  }

  onSubmit(): void {
    if (this.userForm.invalid) {
      this.errorMessage = 'Please correct the errors in the form.';
      this.userForm.markAllAsTouched(); 
      return;
    }

    this.isLoading = true;
    this.errorMessage = null;
    this.successMessage = null;

    const formData: RegisterDto = {
       ...this.userForm.value,
       age: Number(this.userForm.value.age) 
    };

    this.userService.registerUser(formData).subscribe({
      next: (response) => {
        this.isLoading = false;
        this.successMessage = response.message || 'User registered successfully!';
        if (response.status === 'Success' && response.userId) {
            console.log('User registered with ID:', response.userId);
        }
        this.userForm.reset(); 
        setTimeout(() => this.router.navigate(['/users']), 1500); 
      },
      error: (err) => {
        this.isLoading = false;
        this.errorMessage = err.message || 'Failed to register user.'; 
      }
    });
  }

  get name() { return this.userForm.get('name'); }
  get username() { return this.userForm.get('username'); }
  get password() { return this.userForm.get('password'); }
  get age() { return this.userForm.get('age'); }
  get role() { return this.userForm.get('role'); }
  get gender() { return this.userForm.get('gender'); }
  get email() { return this.userForm.get('email'); }
  get webpage() { return this.userForm.get('webpage'); }
}
