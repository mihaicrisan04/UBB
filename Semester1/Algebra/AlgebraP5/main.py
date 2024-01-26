import sympy as sp
from itertools import product
import numpy as np

class Matrix:
    def __init__(self, n, m, matrix):
        self.n = n
        self.m = m
        self.matrix = np.reshape(matrix, (n, m))

    def __str__(self):
        res = ""
        for index, row in enumerate(self.matrix):
            if index == 0:
                res += "\t("
            else:
                res += "\t "
            for elem in row:
                res += str(elem) + " "
            res += "\n"
        res = res[:-1]  # Remove last \n
        res = res[:-1]  # Remove last space
        res += ")"
        return res

    def __eq__(self, other):
        if not isinstance(other, Matrix):
            return False
        for i in range(self.n):
            for j in range(self.m):
                if self.matrix[i][j] != other.matrix[i][j]:
                    return False
        return True

def generate(n, m):
    matrices = list(product(range(2), repeat=n*m))
    matrices = np.array(matrices)
    matrices = matrices.reshape(len(matrices), n, m)
    return matrices


def main():
    for i in range(1,6):
        input_file = open(f"input/input{i}.txt", "r")
        output_file = open(f"output/output{i}.txt", "w")

        line = input_file.readline()
        line = [int(x) for x in line.split() if x != ' ']

        n, m = int(line[0]), int(line[1])

        matrices = generate(n, m)

        refs = []
        for matrix in matrices:
            matrix = sp.Matrix(matrix)

            ref = matrix.rref()
            ref = ref[0]

            refs.append(ref)

        ref_set = set(tuple(matrix) for matrix in refs)
        
        # Remove matrices with -1 becase we are in Z_2
        elem_to_remove = []
        for ref in ref_set:
            if any(elem not in [0, 1] for elem in ref):
                elem_to_remove.append(ref)

        for ref in elem_to_remove:
            ref_set.remove(ref)

        # Convert to list of matrices the corect forms
        matrices = []
        for ref in ref_set:
            matrices.append(Matrix(n, m, list(ref)))

        output_file.write(f"1. The number of matrices M_{n},{m}(Z2): {len(matrices)}\n")    
        if n <= 5 and m <= 5:
            output_file.write("2. The matrices are:\n")
            for matrix in matrices:
                output_file.write(str(matrix) + "\n")
                output_file.write("-"*20 + "\n")

        input_file.close()
        output_file.close()

main()