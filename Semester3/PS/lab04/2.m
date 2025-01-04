clc; clear;

function data = simulate_bernoulli(p, size)
    data = rand(1, size) < p; % Generates 1 if random number is less than p, else 0
end

function data = simulate_binomial(n, p, size)
    data = sum(rand(n, size) < p); % Simulate n Bernoulli trials and sum for Binomial
end

function data = simulate_geometric(p, size)
    data = floor(log(rand(1, size)) ./ log(1 - p)); % Inverse CDF method
end

function data = simulate_pascal(n, p, size)
    data = sum(simulate_geometric(p, size) < n, 1) - 1; % Sum of n geometric trials
end


% Parameters
p = 0.5; % Probability of success
n = 10;  % Number of trials for Binomial and Pascal
size = 1000; % Sample size

% Simulate distributions
bernoulli_data = simulate_bernoulli(p, size);
binomial_data = simulate_binomial(n, p, size);
geometric_data = simulate_geometric(p, size);
pascal_data = simulate_pascal(n, p, size);

% Plot results
figure;
subplot(2, 2, 1);
histogram(bernoulli_data, 2, 'Normalization', 'probability', 'FaceColor', 'blue');
title('Bernoulli Distribution');
xlabel('Value');
ylabel('Probability');

subplot(2, 2, 2);
histogram(binomial_data, n+1, 'Normalization', 'probability', 'FaceColor', 'green');
title('Binomial Distribution');
xlabel('Value');
ylabel('Probability');

subplot(2, 2, 3);
histogram(geometric_data, 20, 'Normalization', 'probability', 'FaceColor', 'purple');
title('Geometric Distribution');
xlabel('Value');
ylabel('Probability');

subplot(2, 2, 4);
histogram(pascal_data, 20, 'Normalization', 'probability', 'FaceColor', 'orange');
title('Pascal (Negative Binomial) Distribution');
xlabel('Value');
ylabel('Probability');
