
% Data
premium = [22.4, 24.5, 21.6, 22.4, 24.8, 21.7, 23.4, 23.3, 21.6, 20.0];
regular = [17.7, 14.8, 19.6, 19.6, 12.1, 14.8, 15.4, 12.6, 14.0, 12.2];

% Sample sizes, means, and variances
n1 = length(premium);
n2 = length(regular);
mean1 = mean(premium);
mean2 = mean(regular);
var1 = var(premium, 1); % Sample variance
var2 = var(regular, 1); % Sample variance

% Pooled variance
sp2 = ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2);
sp = sqrt(sp2);

% t critical value
alpha = 0.05;
t_critical_equal = tinv(1 - alpha / 2, n1 + n2 - 2);

% Margin of error
margin_equal = t_critical_equal * sp * sqrt(1 / n1 + 1 / n2);

% Confidence interval
ci_mean_equal = [(mean1 - mean2) - margin_equal, (mean1 - mean2) + margin_equal];
disp('Mean difference CI (Equal variances):');
disp(ci_mean_equal);

% Standard error for the difference
se_diff = sqrt(var1 / n1 + var2 / n2);

% Degrees of freedom (Welch's formula)
df_welch = (var1 / n1 + var2 / n2)^2 / ...
           (((var1 / n1)^2 / (n1 - 1)) + ((var2 / n2)^2 / (n2 - 1)));

% t critical value for unequal variances
t_critical_unequal = tinv(1 - alpha / 2, df_welch);

% Margin of error
margin_unequal = t_critical_unequal * se_diff;

% Confidence interval
ci_mean_unequal = [(mean1 - mean2) - margin_unequal, (mean1 - mean2) + margin_unequal];
disp('Mean difference CI (Unequal variances):');
disp(ci_mean_unequal);

% F critical values
f_critical_low = finv(alpha / 2, n1 - 1, n2 - 1);
f_critical_high = finv(1 - alpha / 2, n1 - 1, n2 - 1);

% Confidence interval for variance ratio
ci_variance_ratio = [(var1 / var2) / f_critical_high, (var1 / var2) / f_critical_low];
disp('Variance ratio CI:');
disp(ci_variance_ratio);