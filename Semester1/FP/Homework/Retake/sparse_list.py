
class SparseListIterator:
    def __init__(self, data):
        self.data = data
        self.index = 0
        self.count = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.count == len(self.data):
            raise StopIteration

        if self.index not in self.data:
            value = 0
        else:
            value = self.data[self.index]
            self.count += 1
        
        self.index += 1
        return value


class SparseList:
    def __init__(self):
        self.data = {}

    def __getitem__(self, index):
        return self.data[index]

    def __setitem__(self, index, value):
        self.data[index] = value

    def __delitem__(self, index):
        del self.data[index]

    def __iter__(self):
        return SparseListIterator(self.data)



if __name__ == '__main__':
    l = SparseList()
    l[0] = 1
    l[2] = 3

    for i in l:
        for j in l:
            print(i, j)

    del l[0]
