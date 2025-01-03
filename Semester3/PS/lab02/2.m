% Parameters
n = 3; % Number of trials (coin tosses)
p = 0.5; % Probability of heads (success)

% a) Probability Distribution Function (PDF)
k = 0:n; % Possible values of X
pdf_values = binopdf(k, n, p); % Binomial PDF

disp('PDF values:');
disp(pdf_values);

% b) Cumulative Distribution Function (CDF)
cdf_values = binocdf(k, n, p); % Binomial CDF

disp('CDF values:');
disp(cdf_values);

% c) Specific probabilities
P_X_0 = binopdf(0, n, p); % P(X = 0)
P_X_not_1 = 1 - binopdf(1, n, p); % P(X ≠ 1)

disp(['P(X = 0): ', num2str(P_X_0)]);
disp(['P(X ≠ 1): ', num2str(P_X_not_1)]);

% d) Cumulative probabilities
P_X_leq_2 = binocdf(2, n, p); % P(X ≤ 2)
P_X_less_2 = binocdf(1, n, p); % P(X < 2)

disp(['P(X ≤ 2): ', num2str(P_X_leq_2)]);
disp(['P(X < 2): ', num2str(P_X_less_2)]);

% e) Complementary probabilities
P_X_geq_1 = 1 - binocdf(0, n, p); % P(X ≥ 1)
P_X_greater_1 = 1 - binocdf(1, n, p); % P(X > 1)

disp(['P(X ≥ 1): ', num2str(P_X_geq_1)]);
disp(['P(X > 1): ', num2str(P_X_greater_1)]);

% f) Simulate coin tosses
num_simulations = 10; % Number of simulations
simulated_tosses = binornd(n, p, [1, num_simulations]);

disp('Simulated coin toss results:');
disp(simulated_tosses);

% Plot PDF and CDF
figure;

% Plot PDF
subplot(1, 2, 1);
bar(k, pdf_values, 'b');
title('Probability Distribution Function (PDF)');
xlabel('Number of Heads (X)');
ylabel('Probability');
grid on;

% Plot CDF
subplot(1, 2, 2);
stairs(k, cdf_values, 'r', 'LineWidth', 1.5);
title('Cumulative Distribution Function (CDF)');
xlabel('Number of Heads (X)');
ylabel('Cumulative Probability');
grid on;