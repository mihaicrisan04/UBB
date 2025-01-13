% Data input
supplier_A = [1021 980 1017 988 1005 998 1014 985 995 1004 1030 1015 995 1023];
supplier_B = [1070 970 993 1013 1006 1002 1014 997 1002 1010 975];

% Calculate variances
var_A = var(supplier_A);
var_B = var(supplier_B);

% Calculate F-statistic
F = var_A/var_B;

% Degrees of freedom
df1 = length(supplier_A) - 1;  % numerator df
df2 = length(supplier_B) - 1;  % denominator df

% Find critical F-value at 5% significance level
F_critical = finv(0.975, df1, df2);  % Using 0.975 for two-tailed test
F_critical_lower = finv(0.025, df1, df2);

% Calculate means for reliability comparison
mean_A = mean(supplier_A);
mean_B = mean(supplier_B);

% Calculate standard deviations for reliability comparison
std_A = std(supplier_A);
std_B = std(supplier_B);

% Calculate coefficient of variation (CV) for reliability comparison
cv_A = (std_A/mean_A) * 100;
cv_B = (std_B/mean_B) * 100;

% Display results
fprintf('Variance of Supplier A: %.2f\n', var_A);
fprintf('Variance of Supplier B: %.2f\n', var_B);
fprintf('F-statistic: %.4f\n', F);
fprintf('Critical F-value (upper): %.4f\n', F_critical);
fprintf('Critical F-value (lower): %.4f\n', F_critical_lower);
fprintf('\nCoefficient of Variation for Supplier A: %.2f%%\n', cv_A);
fprintf('Coefficient of Variation for Supplier B: %.2f%%\n', cv_B);
