import { Component, OnInit, OnDestroy, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { FormControl, ReactiveFormsModule } from '@angular/forms';
import { UserService, User } from '../../user.service';
import { Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, switchMap, catchError, tap } from 'rxjs/operators';

@Component({
  selector: 'app-user-search',
  standalone: true,
  imports: [CommonModule, ReactiveFormsModule], // Need ReactiveFormsModule for FormControl
  templateUrl: './user-search.component.html',
  styleUrls: ['./user-search.component.scss']
})
export class UserSearchComponent implements OnInit, OnDestroy {
  private userService = inject(UserService);
  private router = inject(Router);

  searchControl = new FormControl(''); // Form control for the search input
  searchResults: User[] = [];
  isLoading = false;
  errorMessage: string | null = null;

  // Subject to handle search term changes
  private searchTerms = new Subject<string>();
  private searchSubscription?: Subscription;

  ngOnInit(): void {
    // Initial load (optional: show all users initially or wait for search)
    // this.performSearch(''); // Uncomment to show all initially

    // Subscribe to search term changes with debounce
    this.searchSubscription = this.searchTerms.pipe(
      debounceTime(300),        // Wait 300ms after each keystroke before considering the term
      distinctUntilChanged(),   // Ignore if next search term is same as previous
      tap(() => {               // Reset state before new search
        this.isLoading = true;
        this.errorMessage = null;
        this.searchResults = [];
      }),
      switchMap((term: string) => // Switch to new search observable
        this.userService.searchUsers(term).pipe(
          catchError(err => { // Handle errors within the inner observable
            this.errorMessage = err.message || 'Search failed.';
            this.isLoading = false;
            return []; // Return empty array on error to clear results
          })
        )
      )
    ).subscribe(users => {
      this.isLoading = false;
      this.searchResults = users;
      if (users.length === 0 && !this.errorMessage && this.searchControl.value) {
         // You could set a specific message like "No users found matching [term]"
         // this.errorMessage = "No users found."; // Or keep it empty
      } 
    });

     // Trigger initial search if needed, or push initial value from control
     this.searchTerms.next(this.searchControl.value || '');

     // Also update searchTerms when the form control value changes directly
     this.searchControl.valueChanges.subscribe(term => this.searchTerms.next(term || ''));
  }

  ngOnDestroy(): void {
    // Unsubscribe to prevent memory leaks
    this.searchSubscription?.unsubscribe();
  }

  // Push the search term into the Subject stream
  // This method isn't strictly needed anymore as we subscribe to valueChanges,
  // but can be kept if triggered by a button click, for example.
  // search(term: string): void {
  //   this.searchTerms.next(term);
  // }

  // --- Methods for Edit/Delete (reuse from UserListComponent logic) ---
  editUser(id: number): void {
    this.router.navigate(['/users/edit', id]);
  }

  deleteUser(id: number): void {
    if (confirm('Are you sure you want to delete this user?')) {
      // Show loading specific to deletion maybe?
      this.userService.deleteUser(id).subscribe({
        next: (res) => {
          console.log(res.message); 
          // Refresh the current search results after deletion
          this.searchTerms.next(this.searchControl.value || ''); 
        },
        error: (err) => {
          this.errorMessage = err.message || 'Failed to delete user.';
          // Potentially clear loading state if deletion had one
        }
      });
    }
  }
}
