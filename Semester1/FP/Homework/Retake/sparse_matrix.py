


class SparseMatrix:
    def __init__(self, n, m):   
        self.n = n
        self.m = m
        self.matrix = {}

    def __str__(self):
        string = ""
        for i in range(self.n):
            for j in range(self.m):
                if (i, j) in self.matrix:
                    string += str(self.get(i, j)) + " "
                else:
                    string += "0 "
            string += "\n"
        return string

    def set(self, i, j, value):
        if i < 0 or i >= self.n or j < 0 or j >= self.m:
            raise ValueError("Index out of range")
        
        self.matrix[(i, j)] = value

    def get(self, i, j):
        if i < 0 or i >= self.n or j < 0 or j >= self.m:
            raise ValueError("Index out of range")

        return self.matrix[(i, j)]


m1 = SparseMatrix(3, 3)
m1.set(1, 1, 2)
m1.set(2, 2, 4)
print(m1)
try:
    m1.set(3, 3, 5)
except ValueError as e:
    print(type(e))
m1.set(1, 1, m1.get(1, 1) + 1)
print(m1)



