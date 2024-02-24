

class X:
    def f(self):
        print("X")


class Y(X):
    def __init__(self, a):
        self.__a = a

    def f(self):
        self.__a.f()
        print("Y")


class Z(Y):
    def __init__(self, a):
        super().__init__(a)


for o in [Y(Y(X())), Z(Y(X()))]:
    o.f()