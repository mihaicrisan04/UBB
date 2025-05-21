import { AbstractControl, ValidationErrors, ValidatorFn } from '@angular/forms';

/**
 * Custom validator to check if two form fields match.
 * @param controlName The name of the first control.
 * @param matchingControlName The name of the control to match against.
 */
export function passwordMatchValidator(controlName: string, matchingControlName: string): ValidatorFn {
  return (formGroup: AbstractControl): ValidationErrors | null => {
    const control = formGroup.get(controlName);
    const matchingControl = formGroup.get(matchingControlName);

    if (!control || !matchingControl) {
      return null; // Or throw an error if controls are not found
    }

    // Set error on matchingControl if validation fails, to display error message there
    if (matchingControl.errors && !matchingControl.errors['passwordMismatch']) {
      // Return null if there are other errors on matchingControl
      return null;
    }

    if (control.value !== matchingControl.value) {
      matchingControl.setErrors({ passwordMismatch: true });
      return { passwordMismatch: true }; // Return error for the form group as well
    } else {
      // If values match, clear the error from matchingControl
      // Check if the only error is passwordMismatch before clearing
      if (matchingControl.hasError('passwordMismatch')) {
        const errors = { ...matchingControl.errors };
        delete errors['passwordMismatch'];
        if (Object.keys(errors).length === 0) {
          matchingControl.setErrors(null);
        } else {
          matchingControl.setErrors(errors);
        }
      }
      return null;
    }
  };
} 