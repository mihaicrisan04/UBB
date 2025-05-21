import { inject } from '@angular/core';
import { CanActivateFn, Router } from '@angular/router';
import { AuthService } from './auth.service';
import { map, take } from 'rxjs/operators';

export const authGuard: CanActivateFn = (route, state) => {
  const authService = inject(AuthService);
  const router = inject(Router);

  return authService.isAuthenticated$.pipe(
    take(1), // Take the latest value and complete
    map(isAuthenticated => {
      if (isAuthenticated) {
        return true; // User is logged in, allow access
      }
      // User is not logged in, redirect to login page
      // Pass the attempted URL as a query parameter for potential redirect after login
      router.navigate(['/login'], { queryParams: { returnUrl: state.url } });
      return false;
    })
  );
};

// Optional: A guard to prevent logged-in users from accessing login/register pages
export const publicPagesGuard: CanActivateFn = (route, state) => {
  const authService = inject(AuthService);
  const router = inject(Router);

  return authService.isAuthenticated$.pipe(
    take(1),
    map(isAuthenticated => {
      if (isAuthenticated) {
        // User is logged in, redirect them away from login/register (e.g., to home)
        router.navigate(['/']); 
        return false;
      }
      return true; // User is not logged in, allow access to login/register
    })
  );
}; 