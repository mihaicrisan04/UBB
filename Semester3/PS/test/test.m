clc;
clear all;
pause(0.1);


data =[1001.7, 975.0, 978.3, 988.3, 978.7, 988.9, 1000.3, 979.2, 968.9, 983.5, 999.2, 985.6];

% sample size
n = length(data);

% sample mean and standard deviation
x_bar = mean(data); % mean
s = std(data); % standard deviation

% confidence interval (95%)
% the ci is the interval where the "true" mean is situated in, with 95% confidence 
alpha = 0.05; % significance level for 95% (95% of the time the true mean will be situatted in the interval)
t_critical = tinv(1 - alpha/2, n-1); % critical t-value
margin_of_error = t_critical * (s / sqrt(n));
CI_lower = x_bar - margin_of_error;
CI_upper = x_bar + margin_of_error;

% hypothesis test (H0(null hypothesis): mu = 995, Ha(alternate hypothesis): mu > 995, at 1% significance (the treshold used in hypothesis testing))
mu_0 = 995; % (H0(null hypoth))
alpha_hypothesis = 0.01; % 1% significance level
df = n - 1; % degrees of freedom

% calculate t-statistic
t_statistic = (x_bar - mu_0) / (s / sqrt(n));

% critical t-value for one-tailed test
t_critical = tinv(1 - alpha_hypothesis, df);

% p-value
p_value = 1 - tcdf(t_statistic, df);


% results
fprintf('Sample Mean: %.2f m/s\n', x_bar);
fprintf('Sample Standard Deviation: %.2f m/s\n', s);

% results confidence interval
fprintf('95%% Confidence Interval: [%.3f, %.3f]\n', CI_lower, CI_upper);

% results test
fprintf('\nt-Statistic: %.2f\n', t_statistic);
fprintf('Critical t-Value: %.2f\n', t_critical);
fprintf('Rejection Region: > %.2f\n', t_critical);
fprintf('p-Value: %.4f\n', p_value);

if t_statistic > t_critical
    fprintf('Reject the null hypothesis: The data suggests the muzzle velocities are faster than 995 m/s.\n');
else
    fprintf('Fail to reject the null hypothesis: The data does not suggest the muzzle velocities are faster than 995 m/s.\n');
end

% OUTPUT OF CODE
% Sample Mean: 985.63 m/s
% Sample Standard Deviation: 10.51 m/s
% 95% Confidence Interval: [978.953, 992.314]
% 
% t-Statistic: -3.09
% Critical t-Value: 2.72
% Rejection Region: > 2.72
% p-Value: 0.9948
% Fail to reject the null hypothesis: The data does not suggest the muzzle velocities are faster than 995 m/s.
