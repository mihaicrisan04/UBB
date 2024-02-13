

class Complex:
    def __init__(self, real, imag = 0):
        self.num = {'real': real, 'imag': imag}

    def __add__(self, other):
        return Complex(self.num['real'] + other.num['real'], self.num['imag'] + other.num['imag'])

    def __mul__(self, other):
        return Complex(self.num['real'] * other.num['real'] - self.num['imag'] * other.num['imag'],
                       self.num['real'] * other.num['imag'] + self.num['imag'] * other.num['real'])

    def __repr__(self):
        string = ''
        if self.num['real'] != 0:
            string += str(self.num['real'])
        if self.num['imag'] != 0:
            if self.num['imag'] > 0:
                if self.num['real'] != 0:
                    string += '+'
                if self.num['imag'] != 1:
                    string += str(self.num['imag'])
                string += 'i'
            else:
                if self.num['imag'] != -1:
                    string += str(self.num['imag'])
                string += 'i'
        if string == '':
            string = '0'
        return string



if __name__ == '__main__':
    z1 = Complex(1, 2)
    z2 = Complex(3, 4)
    print(z1 + z2)
    print(z1 * z2)
    print(z1)
    print(z2)
    print(Complex(0, 0))
    print(Complex(0, 1))
    print(Complex(0, -1))
    print(Complex(1, 0))
    # Test that were givcen in the written exam?...



