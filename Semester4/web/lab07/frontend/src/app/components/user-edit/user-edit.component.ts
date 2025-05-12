import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { UserService, User } from '../../user.service';
import { switchMap } from 'rxjs/operators';
import { Observable, of } from 'rxjs';

@Component({
  selector: 'app-user-edit',
  standalone: true,
  imports: [CommonModule, ReactiveFormsModule],
  templateUrl: './user-edit.component.html',
  styleUrls: ['./user-edit.component.scss']
})
export class UserEditComponent implements OnInit {
  private fb = inject(FormBuilder);
  private userService = inject(UserService);
  private route = inject(ActivatedRoute);
  private router = inject(Router);

  userForm: FormGroup;
  userId: number | null = null;
  isLoading = false;
  isFetching = false; // Separate loading state for initial fetch
  errorMessage: string | null = null;
  successMessage: string | null = null;

  roles: string[] = ['admin', 'user', 'manager', 'employee'];
  genders: string[] = ['male', 'female', 'other'];

  constructor() {
    // Initialize the form structure, similar to add component
    // Password is not included here as we don't fetch it and update is separate
    this.userForm = this.fb.group({
      id: [null], // Keep track of the user ID
      name: ['', [Validators.required, Validators.minLength(2)]],
      username: ['', [Validators.required, Validators.minLength(3)]],
      age: [null, [Validators.required, Validators.min(1), Validators.max(120)]],
      role: ['', Validators.required],
      gender: ['', Validators.required],
      email: ['', [Validators.required, Validators.email]],
      webpage: ['', [Validators.pattern('^(https?://.+)?$')]],
      profile: ['']
    });
  }

  ngOnInit(): void {
    this.isFetching = true;
    this.route.paramMap.pipe(
      switchMap(params => {
        const id = params.get('id');
        if (id) {
          this.userId = +id; // Convert string id to number
          return this.userService.getUser(this.userId);
        } else {
          // No ID found, handle error or redirect
          this.errorMessage = 'No User ID provided in the route.';
          this.isFetching = false;
          return of(null); // Return an empty observable or handle differently
        }
      })
    ).subscribe({
      next: (user) => {
        if (user) {
          // Populate the form with fetched user data
          // Exclude fields not in the form (like password, created_at)
          const { password, created_at, ...formData } = user;
          this.userForm.patchValue(formData); 
        } else if (!this.errorMessage) {
          // Handle case where ID was valid but user not found by service
          this.errorMessage = `User with ID ${this.userId} not found.`;
        }
        this.isFetching = false;
      },
      error: (err) => {
        this.errorMessage = err.message || 'Failed to fetch user data.';
        this.isFetching = false;
      }
    });
  }

  onSubmit(): void {
    if (this.userForm.invalid || !this.userId) {
      this.errorMessage = 'Please correct the errors in the form.';
      this.userForm.markAllAsTouched();
      return;
    }

    this.isLoading = true;
    this.errorMessage = null;
    this.successMessage = null;

    const updatedUserData: User = {
      ...this.userForm.value,
      id: this.userId, // Ensure the ID is included
      age: Number(this.userForm.value.age) // Ensure age is number
    };

    this.userService.updateUser(updatedUserData).subscribe({
      next: (response) => {
        this.isLoading = false;
        this.successMessage = response.message || 'User updated successfully!';
         // Optionally navigate away after a delay
        setTimeout(() => this.router.navigate(['/users']), 1500);
      },
      error: (err) => {
        this.isLoading = false;
        this.errorMessage = err.message || 'Failed to update user.';
      }
    });
  }

  // Helper getters for template validation
  get name() { return this.userForm.get('name'); }
  get username() { return this.userForm.get('username'); }
  get age() { return this.userForm.get('age'); }
  get role() { return this.userForm.get('role'); }
  get gender() { return this.userForm.get('gender'); }
  get email() { return this.userForm.get('email'); }
  get webpage() { return this.userForm.get('webpage'); }

}
