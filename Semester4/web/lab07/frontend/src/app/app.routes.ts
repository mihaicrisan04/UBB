import { Routes } from '@angular/router';

// Import the generated components
import { UserListComponent } from './components/user-list/user-list.component';
import { UserAddComponent } from './components/user-add/user-add.component';
import { UserEditComponent } from './components/user-edit/user-edit.component';
import { UserSearchComponent } from './components/user-search/user-search.component';

export const routes: Routes = [
    // Default route redirects to user list
    { path: '', redirectTo: '/users', pathMatch: 'full' }, 
    // Browse users (list)
    { path: 'users', component: UserListComponent, title: 'Browse Users' }, 
    // Add user form
    { path: 'users/add', component: UserAddComponent, title: 'Add User' }, 
    // Edit user form (uses route parameter :id)
    { path: 'users/edit/:id', component: UserEditComponent, title: 'Edit User' }, 
    // Search users page
    { path: 'search', component: UserSearchComponent, title: 'Search Users' },
    // Wildcard route for 404 - can create a dedicated NotFoundComponent later
    { path: '**', redirectTo: '/users' } // Or redirect to a 404 component
];
