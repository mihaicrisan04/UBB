% Problem 1: Continuous Random Variables
clc; clear;

% Define the parameters for the distributions
mu = 0;      % Mean for Normal distribution
sigma = 1;   % Standard deviation for Normal distribution
alpha = 0.25; % Quantile for lower tail
beta = 0.1;   % Quantile for upper tail

% a) P(X <= 0) and P(X >= 0) for Normal distribution
P_X_leq_0 = normcdf(0, mu, sigma);   % CDF at 0
P_X_geq_0 = 1 - P_X_leq_0;          % Complement

% b) P(-1 <= X <= 1) and P(X <= -1 or X >= 1)
P_X_in_range = normcdf(1, mu, sigma) - normcdf(-1, mu, sigma);
P_X_out_range = 1 - P_X_in_range;

% c) Find x_alpha such that P(X < x_alpha) = alpha
x_alpha = norminv(alpha, mu, sigma); % Inverse CDF

% d) Find x_beta such that P(X > x_beta) = beta
x_beta = norminv(1 - beta, mu, sigma); % Complementary quantile

% Display results
fprintf('a) P(X <= 0): %.4f, P(X >= 0): %.4f\n', P_X_leq_0, P_X_geq_0);
fprintf('b) P(-1 <= X <= 1): %.4f, P(X <= -1 or X >= 1): %.4f\n', P_X_in_range, P_X_out_range);
fprintf('c) x_alpha for alpha = %.2f: %.4f\n', alpha, x_alpha);
fprintf('d) x_beta for beta = %.2f: %.4f\n', beta, x_beta);