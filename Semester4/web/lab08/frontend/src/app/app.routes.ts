import { Routes } from '@angular/router';

// Import the generated components
import { UserListComponent } from './components/user-list/user-list.component';
import { UserAddComponent } from './components/user-add/user-add.component';
import { UserEditComponent } from './components/user-edit/user-edit.component';
import { UserSearchComponent } from './components/user-search/user-search.component';

// Auth components and guards
import { LoginComponent } from './components/login/login.component';
import { RegisterComponent } from './components/register/register.component';
import { authGuard, publicPagesGuard } from './auth.guard';

export const routes: Routes = [
    // Auth routes
    {
        path: 'login',
        component: LoginComponent,
        canActivate: [publicPagesGuard], // Prevent logged-in users from accessing login page
        title: 'Login'
    },
    {
        path: 'register',
        component: RegisterComponent,
        canActivate: [publicPagesGuard], // Prevent logged-in users from accessing register page
        title: 'Register'
    },

    // Application routes (protected)
    {
        path: 'users',
        component: UserListComponent,
        canActivate: [authGuard], // Protect this route
        title: 'Browse Users'
    },
    {
        path: 'users/add',
        component: UserAddComponent,
        canActivate: [authGuard], // Protect this route
        title: 'Add User'
    },
    {
        path: 'users/edit/:id',
        component: UserEditComponent,
        canActivate: [authGuard], // Protect this route
        title: 'Edit User'
    },
    {
        path: 'search',
        component: UserSearchComponent,
        canActivate: [authGuard], // Protect this route
        title: 'Search Users'
    },

    // Default route: redirect to users list if logged in, or login page if not (handled by authGuard on /users)
    { path: '', redirectTo: '/users', pathMatch: 'full' }, 

    // Wildcard route for 404 - redirect to /users (which will then redirect to /login if not auth)
    // Consider a dedicated NotFoundComponent for a better user experience
    { path: '**', redirectTo: '/users' } 
];
