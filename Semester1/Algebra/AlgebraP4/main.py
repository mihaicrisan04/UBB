# Author: Mihai Crisan
# Date: 25-11-2023

import numpy as np


def generate_vectors(n: int) -> list:
    """
    Generates all the possible vectors of the vector space from Zn over Z2

    :param n: the dimension of the vector space

    :return: list of all the vectors of the vector space Zn over Z2
    """
    def generate(n: int, vector: list) -> None:
        if len(vector) == n:
            vectors.append(vector[:])
        else:
            vector.append(0)
            generate(n, vector)
            vector.pop()
            vector.append(1)
            generate(n, vector)
            vector.pop()

    vectors = []
    generate(n, [])
    return vectors


def get_k_subspaces(n, k) -> int:
    """
    Calculates the number of k-dimensional subspaces of the vector space Zn over Z2

    :param n: the dimension of the vector space
    :param k: the dimension of the subspace

    :return: the number of k-dimensional subspaces of the vector space Zn over Z2
    """
    p2n = 2 ** n
    p2k = 2 ** k
    a, b = 1, 1

    for i in range(0, k):
        a *= (p2n - 2 ** i)
        b *= (p2k - 2 ** i)

    return (a // b)


def gaussian_elimination_Z2(matrix, n, k):
    # perform Gaussian Elimination
    i, j = 0, 0
    while i < k and j < n:
        max_row = i
        # first non-zero element in current column
        while max_row < k and matrix[max_row][j] == 0:
            max_row += 1
 
        # if no non-zero element is found, move to the next column
        if max_row == k:
            j += 1
            continue
 
        # swap rows to move the non-zero element to the current row
        matrix[i], matrix[max_row] = matrix[max_row], matrix[i]
 
        # zero out element from the same column, but from other rows
        for row in range(k):
            if row != i and matrix[row][j] != 0:
                for col in range(j, n):
                    matrix[row][col] = (matrix[row][col] - matrix[i][col]) % 2  # obtain Z2 values
 
        i += 1
        j += 1
 
    return matrix


def get_rank(matrix):
    m, n = matrix.shape
    rank = 0

    for row in matrix:
        if np.count_nonzero(row) > 0:
            rank += 1

    return rank


def generate_combinations(vectors, k):
    combinations = []

    def generate(index, combination):
        if len(combination) == k:
            combinations.append(combination[:])
        else:
            for i in range(index, len(vectors)):
                combination.append(vectors[i])
                generate(i + 1, combination)
                combination.pop()

    generate(0, [])
    return combinations


def matrix_in_matricies(matrix, matricies):
    for m in matricies:
        m = np.array(m)
        matrix = np.array(matrix)
        if np.array_equal(matrix, m):
            return True
    return False


def main() -> None:
    for i in range(1,6):
        input_file = open(f"input/input{i}.txt", "r")
        output_file = open(f"output/output{i}.txt", "w")

        line = input_file.readline()
        line = [int(x) for x in line.split() if x != ' ']

        k, n = int(line[0]), int(line[1])

        if k > n:
            print("k must be less than or equal to n")
            return

        output_file.write(f'The number of {k}-dimnesional subspaces of the vector space Z{n}_2 over Z_2 is: {get_k_subspaces(n, k)}\n')
        if n <= 6:
            vectors = generate_vectors(n)
            vectors = vectors[1:] # remove the zero vector

            combinations = generate_combinations(vectors, k)
            matricies = []

            for combination in combinations:
                matrix = np.array(combination)
                matrix = gaussian_elimination_Z2(matrix, n, k)
                rank = get_rank(matrix)
                if matrix_in_matricies(matrix, matricies) == False and rank == k:
                    matricies.append(matrix)

            for matrix in matricies:
                matrix = '(' +  ' '.join(map(str, matrix)) + ')'
                output_file.write(matrix + '\n')

        input_file.close()
        output_file.close()


if __name__ == "__main__":
    main()
