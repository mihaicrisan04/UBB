import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { UserService, User } from '../../user.service';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-user-list',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './user-list.component.html',
  styleUrls: ['./user-list.component.scss']
})
export class UserListComponent implements OnInit {
  private userService = inject(UserService);
  private router = inject(Router);

  users: User[] = [];
  isLoading = false;
  errorMessage: string | null = null;
  
  selectedRole: string = '';
  previousFilter: string = 'None';
  currentFilter: string = 'None';
  roles: string[] = ['Admin', 'User', 'Editor'];

  ngOnInit(): void {
    this.loadUsers();
  }

  loadUsers(): void {
    this.previousFilter = this.currentFilter;

    this.isLoading = true;
    this.errorMessage = null;

    this.userService.getUsers(this.selectedRole || undefined).subscribe({
      next: (data) => {
        this.users = data;
        this.isLoading = false;
        this.currentFilter = this.selectedRole ? this.selectedRole : 'All Roles';
      },
      error: (err) => {
        this.errorMessage = err.message || 'Failed to load users.';
        this.isLoading = false;
        this.users = [];
      }
    });
  }

  onFilterChange(): void {
    this.loadUsers();
  }

  editUser(id: string): void {
    this.router.navigate(['/users/edit', id]);
  }

  deleteUser(id: string): void {
    if (confirm('Are you sure you want to delete this user?')) {
      this.isLoading = true;
      this.userService.deleteUser(id).subscribe({
        next: (res) => {
          console.log(res.message);
          this.loadUsers();
        },
        error: (err) => {
          this.errorMessage = err.message || 'Failed to delete user.';
          this.isLoading = false;
        }
      });
    }
  }
}
