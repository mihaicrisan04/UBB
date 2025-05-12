import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router, RouterLink } from '@angular/router';
import { UserService, User } from '../../user.service';
import { FormsModule } from '@angular/forms';

@Component({
  selector: 'app-user-list',
  standalone: true,
  imports: [CommonModule, RouterLink, FormsModule],
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
  roles: string[] = ['admin', 'user', 'manager', 'employee'];

  ngOnInit(): void {
    this.loadUsers();
  }

  loadUsers(): void {
    this.isLoading = true;
    this.errorMessage = null;
    this.previousFilter = this.selectedRole ? this.selectedRole : 'All Roles';

    this.userService.getUsers(this.selectedRole || undefined).subscribe({
      next: (data) => {
        this.users = data;
        this.isLoading = false;
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

  editUser(id: number): void {
    this.router.navigate(['/users/edit', id]);
  }

  deleteUser(id: number): void {
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
