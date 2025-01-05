% Section A: Correlation and Regression
% Data
X = [20, 21, 22, 23, 24, 25, 26, 27];
f_X = [2, 1, 3, 6, 5, 9, 2, 2];

Y = [75, 76, 77, 78, 79, 80, 81, 82];
f_Y = [3, 2, 2, 5, 8, 8, 1, 1];

% Means
mean_X = sum(X .* f_X) / sum(f_X);
mean_Y = sum(Y .* f_Y) / sum(f_Y);

% Variances
var_X = sum(f_X .* (X - mean_X).^2) / sum(f_X);
var_Y = sum(f_Y .* (Y - mean_Y).^2) / sum(f_Y);

% Covariance
cov_XY = sum(f_X .* f_Y .* (X - mean_X) .* (Y - mean_Y)) / (sum(f_X) * sum(f_Y));

% Correlation coefficient
corr_coef = cov_XY / sqrt(var_X * var_Y);

% Display results
disp('Section A Results:');
disp(['Mean of X: ', num2str(mean_X)]);
disp(['Mean of Y: ', num2str(mean_Y)]);
disp(['Variance of X: ', num2str(var_X)]);
disp(['Variance of Y: ', num2str(var_Y)]);
disp(['Covariance of X and Y: ', num2str(cov_XY)]);
disp(['Correlation Coefficient: ', num2str(corr_coef)]);