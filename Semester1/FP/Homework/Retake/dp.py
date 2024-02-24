# DYNAMIC PROGRAMMING


def longest_common_subsequence(X, Y):
    m = len(X)
    n = len(Y)

    # Initialize a table to store the lengths of LCS
    L = [[0] * (n + 1) for _ in range(m + 1)]

    # Building the table in a bottom-up manner
    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if X[i - 1] == Y[j - 1]:
                L[i][j] = L[i - 1][j - 1] + 1
            else:
                L[i][j] = max(L[i - 1][j], L[i][j - 1])

    # Length of LCS is stored in L[m][n]
    length_lcs = L[m][n]

    # Backtrack to find the LCS itself
    lcs = []
    i, j = m, n
    while i > 0 and j > 0:
        if X[i - 1] == Y[j - 1]:
            lcs.append(X[i - 1])
            i -= 1
            j -= 1
        elif L[i - 1][j] > L[i][j - 1]:
            i -= 1
        else:
            j -= 1

    # Reverse the LCS since we found it backwards
    lcs.reverse()

    return length_lcs, ''.join(lcs)

# Example usage:
X = "MNPNQMN"
Y = "NQPMNM"
length_lcs, lcs = longest_common_subsequence(X, Y)
print("Length of LCS:", length_lcs)
print("One of the longest common subsequences:", lcs)

##############################################
print()


def rod_cutting(n, prices):
    # Create a matrix to store maximum profit for rods of different lengths
    dp = [[0] * (n + 1) for _ in range(n + 1)]

    # Initialize the matrix
    for i in range(1, n + 1):
        for j in range(1, n + 1):
            if j >= i:
                dp[i][j] = max(dp[i - 1][j], prices[i] + dp[i][j - i])

    return dp[n][n]

# Example usage:
n = 7
prices = [0, 1, 5, 8, 9, 10, 17, 17]  # 0 is added for convenience to match indexes
profit = rod_cutting(n, prices)

print("Length of the rod:", n)
print("Prices of pieces of the rod:", prices[1:])
print("Maximum profit by cutting the rod:", profit)


##############################################
print()

def coin_change(coins, amount):
    # Create a list to store the minimum number of coins needed for each amount
    dp = [float('inf')] * (amount + 1)

    # 0 coins are needed for 0 amount
    dp[0] = 0

    # Calculate the minimum number of coins needed for each amount
    for i in range(1, amount + 1):
        for coin in coins:
            if i - coin >= 0:
                dp[i] = min(dp[i], dp[i - coin] + 1)

    return dp[amount] if dp[amount] != float('inf') else -1

# Example usage:
coins = [1, 2, 5]
amount = 11
print("Coins:", coins)
print("Minimum number of coins needed to make", amount, "is:", coin_change(coins, amount))


##############################################
