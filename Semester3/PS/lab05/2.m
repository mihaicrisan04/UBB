
% Section B: Confidence Intervals
% Data
X = [7, 7, 4, 5, 9, 9, 4, 12, 8, 1, 8, 7, 3, 13, 2, 1, 17, 7, 12, 5, 6, 2, 1, 13, 14, 10, 2, 4, 9, 11, 3, 5, 12, 6, 10, 7];
n = length(X);
alpha = 0.05; % 95% confidence

% Sample mean and variance
mean_X = mean(X);
var_X = var(X, 1); % Population variance (n instead of n-1)
std_X = std(X, 1); % Population standard deviation

% 1a. Confidence interval for the mean (σ known)
sigma = 5; % Given standard deviation
z_critical = norminv(1 - alpha / 2); % z critical value
margin_known = z_critical * sigma / sqrt(n);
ci_mean_known = [mean_X - margin_known, mean_X + margin_known];

% 1b. Confidence interval for the mean (σ unknown)
t_critical = tinv(1 - alpha / 2, n - 1); % t critical value
margin_unknown = t_critical * std_X / sqrt(n);
ci_mean_unknown = [mean_X - margin_unknown, mean_X + margin_unknown];

% 1c. Confidence interval for the variance
chi2_low = chi2inv(alpha / 2, n - 1);
chi2_high = chi2inv(1 - alpha / 2, n - 1);
ci_variance = [(n - 1) * var_X / chi2_high, (n - 1) * var_X / chi2_low];
ci_std = sqrt(ci_variance); % For standard deviation

% Display results
disp('Section B Results:');
disp(['CI for mean (σ known): ', num2str(ci_mean_known)]);
disp(['CI for mean (σ unknown): ', num2str(ci_mean_unknown)]);
disp(['CI for variance: ', num2str(ci_variance)]);
disp(['CI for standard deviation: ', num2str(ci_std)]);