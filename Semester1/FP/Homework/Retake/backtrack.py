# BACKTRACKING


def is_prime(num):
    if num == 0 or num == 1 or (num > 2 and num % 2 == 0):
        return False
    if num == 2 or num == 3:
        return True
    for i in range(3, int(num ** 0.5) + 1, 2):
        if num % i == 0:
            return False
    return True

def decompose_as_primes(n, primes, current_decomposition, all_decompositions):
    if n == 0:
        all_decompositions.append(current_decomposition[:])
        return
    for prime in primes:
        if prime > n:
            break
        if current_decomposition and prime < current_decomposition[-1]:
            continue
        current_decomposition.append(prime)
        decompose_as_primes(n - prime, primes, current_decomposition, all_decompositions)
        current_decomposition.pop()

def find_decompositions(n):
    primes = [i for i in range(2, n) if is_prime(i)]
    all_decompositions = []
    decompose_as_primes(n, primes, [], all_decompositions)
    return all_decompositions

# Example usage:
n = 10
decompositions = find_decompositions(n)
print("All decompositions of", n, "as sums of prime numbers:")
for decomposition in decompositions:
    print(decomposition)


##############################################
print()


def generate_valid_parentheses(n):
    def backtrack(curr, open_count, close_count):
        if len(curr) == n:
            result.append(curr)
            return
        if open_count < n // 2:
            backtrack(curr + "(", open_count + 1, close_count)
        if close_count < open_count:
            backtrack(curr + ")", open_count, close_count + 1)

    result = []
    backtrack("", 0, 0)
    return result

# Example usage:
n = 4  # n must be even
valid_sequences = generate_valid_parentheses(n)
print(f"All sequences of {n} parentheses that close correctly:")
for seq in valid_sequences:
    print(seq)


##############################################
print()


def is_safe(board, row, col, N):
    # Check if there is a queen in the same column
    for i in range(row):
        if board[i][col] == 1:
            return False

    # Check upper diagonal on left side
    for i, j in zip(range(row, -1, -1), range(col, -1, -1)):
        if board[i][j] == 1:
            return False

    # Check upper diagonal on right side
    for i, j in zip(range(row, -1, -1), range(col, N)):
        if board[i][j] == 1:
            return False

    return True

def solve_n_queens_util(board, row, N, result):
    if row == N:
        result.append(["".join(["Q" if cell == 1 else "." for cell in row]) for row in board])
        return

    for col in range(N):
        if is_safe(board, row, col, N):
            board[row][col] = 1
            solve_n_queens_util(board, row + 1, N, result)
            board[row][col] = 0

def solve_n_queens(N):
    board = [[0] * N for _ in range(N)]
    result = []
    solve_n_queens_util(board, 0, N, result)
    return result

# Example usage:
N = 4
solutions = solve_n_queens(N)
print(f"All solutions for {N} Queens Problem:")
for i, solution in enumerate(solutions, start=1):
    print(f"Solution {i}:")
    for row in solution:
        print(row)
    print()


##############################################
print()


def permute(nums):
    def backtrack(start):
        if start == len(nums):
            result.append(nums[:])  # Make a copy of the current permutation
            return
        for i in range(start, len(nums)):
            # Swap elements at indices start and i
            nums[start], nums[i] = nums[i], nums[start]
            # Recursively generate permutations for the rest of the list
            backtrack(start + 1)
            # Undo the swap to backtrack
            nums[start], nums[i] = nums[i], nums[start]

    result = []
    backtrack(0)
    return result

# Example usage:
nums = [1, 2, 3]
permutations = permute(nums)
print("All permutations of", nums, "are:")
for perm in permutations:
    print(perm)


##############################################
