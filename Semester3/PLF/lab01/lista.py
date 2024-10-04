class Nod:
    def __init__(self, e):
        self.e = e
        self.urm = None
    
class Lista:
    def __init__(self):
        self.prim = None
      
    '''
    Determine if a list has even number of elements, without computing the length of the list.
                            |  1 - i, n = 0             (1 == pare, 0 == impare)
    f(l1, l2, ..., ln, i) = |  f(2, ..., ln, 1 - i), nod != None
                            |
    '''
    # def evenNumberOfElements(self):
    #     return self.evenNumberOfElementsRec(self.prim, 0)

    # def evenNumberOfElementsRec(self, nod, i):
    #     if nod != None:
    #         return self.evenNumberOfElementsRec(nod.urm, 1 - i)
    #     return 1 - i

    # true false  (true = pare, false = impare)
    #  _    _ 
    # (l1, l2) -> (l3, l4) -> ... -> (ln-1, ln) -> None
    # (1, 2) -> (3, 4) -> (5, _) -> None
    #                        false
    def evenNumberOfElementsRec(self, nod):
        if nod == None:
            return True
        if nod.urm == None:
            return False
        return self.evenNumberOfElementsRec(nod.urm.urm)

    ''' 
    Delete all occurrences of an element e from a list.
                            | [], n = 0
    f(l1, l2, ..., ln, e) = | l1 U f(l2, l3, ..., ln, e), l1 != e
                            | f(l2, l3, ..., ln), l1 == e

    '''
    def deleteAllOccurrences(self, e):
        self.prim = self.deleteAllOccurrencesRec(self.prim, e)

    def deleteAllOccurrencesRec(self, nod, e):
        if nod != None:
            if nod.e == e:
                return self.deleteAllOccurrencesRec(nod.urm, e)
            else:
                nod.urm = self.deleteAllOccurrencesRec(nod.urm, e)
                return nod
        return None

           

'''
crearea unei liste din valori citite pana la 0
'''
def creareLista():
    lista = Lista()
    lista.prim = creareLista_rec()
    return lista

def creareLista_rec():
    x = int(input("x="))
    if x == 0:
        return None
    else:
        nod = Nod(x)
        nod.urm = creareLista_rec()
        return nod
    
'''
tiparirea elementelor unei liste
'''
def tipar(lista):
    tipar_rec(lista.prim)
    
def tipar_rec(nod):
    if nod != None:
        print (nod.e)
        tipar_rec(nod.urm)

'''
program pentru test
'''

def main():
    list = creareLista()
    print("pare" if list.evenNumberOfElementsRec(list.prim) else "impare")
    # x = int(input("de sters="))
    # list.deleteAllOccurrences(x)
    tipar(list)
    
main() 
    