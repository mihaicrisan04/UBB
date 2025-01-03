% Problem 2: Approximations of the Binomial Distribution
clc; clear;

% Parameters for the binomial distribution
n = 50; % Number of trials
p = 0.2; % Probability of success
lambda = n * p; % Mean for Poisson
mu = n * p; % Mean for Normal
sigma = sqrt(n * p * (1 - p)); % Standard deviation for Normal

% Generate values for k (number of successes)
k = 0:n;

% Exact Binomial PMF
binomial_pmf = binopdf(k, n, p);

% Normal Approximation
normal_pdf = normpdf(k, mu, sigma);

% Poisson Approximation
poisson_pmf = poisspdf(k, lambda);

% Plot the distributions
figure;
hold on;
bar(k, binomial_pmf, 'FaceAlpha', 0.5, 'DisplayName', 'Binomial PMF');
plot(k, normal_pdf, 'r-', 'LineWidth', 1.5, 'DisplayName', 'Normal Approximation');
plot(k, poisson_pmf, 'g--', 'LineWidth', 1.5, 'DisplayName', 'Poisson Approximation');
hold off;

% Add labels and legend
xlabel('Number of Successes (k)');
ylabel('Probability');
title('Binomial Distribution and Its Approximations');
legend show;
grid on;

% Display numerical results for first few probabilities
disp('First few probabilities:');
disp(table(k', binomial_pmf', normal_pdf', poisson_pmf', ...
    'VariableNames', {'k', 'Binomial', 'Normal', 'Poisson'}));