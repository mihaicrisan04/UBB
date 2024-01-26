# Author: Mihai Crisan
# Date: 26-02-2021


def print_table(table: list, n: int) -> None:
    for i in range(0, n):
        for j in range(0, n):
            print(table[i][j]+1, end=" ")
        print()
    print("-"*20)


def print_tables(solutions: list, n: int) -> None:
    for i in range(len(solutions)):
        print("Table number: ", i+1)
        print_table(solutions[i], n)


def check_associativity(table: list, n: int) -> bool:
    for i in range(0, n):
        for j in range(0, n):
            for k in range(0, n):
                # check a * (b * c) = (a * b) * c
                # i = a, j = b, k = c
                # table[i][j] = a * b and table[j][k] = b * c and table[table[i][j]][k] = (a * b) * c
                if table[table[i][j]][k] != table[i][table[j][k]]:
                    return False
    return True

def generate_solutions(n: int) -> list:
    def compute_operation_tables(current_table: list, i: int, j: int, n: int) -> None:
        for k in range(0, n):
            current_table[i][j] = k
            if i == n-1 and j == n-1:
                if check_associativity(current_table, n):
                    solutions.append([row[:] for row in current_table])
            else:
                if j == n-1:
                    compute_operation_tables(current_table, i+1, 0, n)
                else:
                    compute_operation_tables(current_table, i, j+1, n)

    solutions = []
    table = [[0 for i in range(0, n)] for j in range(0, n)]
    compute_operation_tables(table, 0, 0, n)
    return solutions


def test() -> None:
    pass

def main() -> None:
    n = int(input("Enter the value of n: "))

    if n <= 4:
        solutions = generate_solutions(n)
        print("Number of associative operations: ", len(solutions))
        print_tables(solutions, n)
    else:
        pass


if __name__ == "__main__":
    main()