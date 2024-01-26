# Author: Mihai Crisan
# Date: 10-11-2023

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

def generate_bases(vectors: list, n: int) -> list:
    """
    Generates all the bases of the vector space from Zn over Z2 with n = 1,2,3,4
    and writes them to the output file
    The method for checking if a basis is valid is by calculating the determinant of the matrix
    formed by the vectors of the basis. If the determinant is not 0, then the basis is valid

    :param vectors: all the vectors of the vector space Zn over Z2
    :param n: the dimension of the vector space

    :return: list of all the bases of the vector space Zn over Z2
    """
    bases = []

    if n == 1:
        bases.append(vectors[1][:])
    if n == 2:
        for i in range(len(vectors)):
            for j in range(len(vectors)):
                basis = []
                basis.append(vectors[i][:])
                basis.append(vectors[j][:])
                matrix = np.array(basis)
                det = np.linalg.det(matrix)
                if det % 2 != 0:
                    bases.append(basis)
    if n == 3:
        for i in range(len(vectors)):
            for j in range(len(vectors)):
                for k in range(len(vectors)):
                    basis = []
                    basis.append(vectors[i][:])
                    basis.append(vectors[j][:])
                    basis.append(vectors[k][:])
                    matrix = np.array(basis)
                    det = np.linalg.det(matrix)
                    if det % 2 != 0:
                        bases.append(basis)
    if n == 4:
        for i in range(len(vectors)):
            for j in range(len(vectors)):
                for k in range(len(vectors)):
                    for l in range(len(vectors)):
                        basis = []
                        basis.append(vectors[i][:])
                        basis.append(vectors[j][:])
                        basis.append(vectors[k][:])
                        basis.append(vectors[l][:])
                        matrix = np.array(basis)
                        det = np.linalg.det(matrix)
                        if det % 2 != 0:
                            bases.append(basis)
    return bases

def main() -> None:
    for i in range(1,6):
        input_file = open(f"input/input{i}.txt", "r")
        output_file = open(f"output/output{i}.txt", "w")

        n = int(input_file.readline())

        if n <= 4:
            vectors = generate_vectors(n)
            bases = generate_bases(vectors, n)
            output_file.write(f"The number of bases of the vector space Z{n} over Z2 is: {len(bases)}\n")
            for basis in bases:
                output_file.write(f"{bases.index(basis) + 1}. ( ") # this line really slows down the program but for small n makes the output more readable
                for vector in basis:
                    output_file.write(str(vector) + " ")
                output_file.write(")\n")
        else:
            num_of_bases = 1
            TWO_TO_PWR_N = 2 ** n
            for i in range(n):
                # the formula deduced from the seminar for calculating the number of bases
                num_of_bases *= TWO_TO_PWR_N - 2 ** i
            output_file.write(f"The number of bases of the vector space Z{n} over Z2 is: {num_of_bases}")

        input_file.close()
        output_file.close()


if __name__ == "__main__":
    main()
