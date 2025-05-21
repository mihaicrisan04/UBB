import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { UserService, User, UpdateUserDto } from '../../user.service';
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
  userId: string | null = null;
  isLoading = false;
  isFetching = false;
  errorMessage: string | null = null;
  successMessage: string | null = null;

  roles: string[] = ['Admin', 'User', 'Editor'];
  genders: string[] = ['Male', 'Female', 'Other'];

  constructor() {
    this.userForm = this.fb.group({
      name: ['', [Validators.required, Validators.minLength(2)]],
      username: [{ value: '', disabled: true }, [Validators.required, Validators.minLength(3)]],
      age: [null, [Validators.required, Validators.min(1), Validators.max(120)]],
      role: ['', Validators.required],
      gender: ['', Validators.required],
      email: [{ value: '', disabled: true }, [Validators.required, Validators.email]],
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
          this.userId = id;
          return this.userService.getUser(this.userId);
        } else {
          this.errorMessage = 'No User ID provided in the route.';
          this.isFetching = false;
          return of(null);
        }
      })
    ).subscribe({
      next: (user) => {
        if (user) {
          const formDataToPatch = {
            name: user.name,
            username: user.username,
            age: user.age,
            role: user.role,
            gender: user.gender,
            email: user.email,
            webpage: user.webpage,
            profile: user.profile
          };
          this.userForm.patchValue(formDataToPatch);
        } else if (!this.errorMessage) {
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
    if (!this.userId) {
      this.errorMessage = 'User ID is missing, cannot update.';
      return;
    }
    if (this.userForm.invalid) {
      this.errorMessage = 'Please correct the errors in the form.';
      this.userForm.markAllAsTouched();
      return;
    }

    this.isLoading = true;
    this.errorMessage = null;
    this.successMessage = null;

    const formValue = this.userForm.value;
    const updatedUserData: UpdateUserDto = {
      name: formValue.name,
      age: Number(formValue.age),
      role: formValue.role,
      gender: formValue.gender,
      webpage: formValue.webpage,
      profile: formValue.profile
    };

    this.userService.updateUser(this.userId, updatedUserData).subscribe({
      next: (response) => {
        this.isLoading = false;
        this.successMessage = response.message || 'User updated successfully!';
        this.userForm.markAsPristine();
        setTimeout(() => this.router.navigate(['/users']), 1500);
      },
      error: (err) => {
        this.isLoading = false;
        this.errorMessage = err.message || 'Failed to update user.';
      }
    });
  }

  get name() { return this.userForm.get('name'); }
  get username() { return this.userForm.get('username'); }
  get age() { return this.userForm.get('age'); }
  get role() { return this.userForm.get('role'); }
  get gender() { return this.userForm.get('gender'); }
  get email() { return this.userForm.get('email'); }
  get webpage() { return this.userForm.get('webpage'); }
}
