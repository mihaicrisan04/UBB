import copy
import random


# Python implementation

class Edge_id:
    def __init__(self, source, target):
        self.source = source
        self.target = target
    
    def __eq__(self, other):
        if not isinstance(other, Edge_id):
            return False
        return self.source == other.source and self.target == other.target

    def __lt__(self, other):
        if not isinstance(other, Edge_id):
            return NotImplemented
        return (self.source, self.target) < (other.source, other.target)

    def __hash__(self): 
        return hash((self.source, self.target))

    def __repr__(self):
        return str(self.source) + " " + str(self.target)


class VertexIterator:
    def __init__(self, n, adjList):
        self.n = n
        self.current = 0
        self.adjList = adjList

    def __iter__(self):
        return self

    def __next__(self):
        if self.current < self.n:
            while self.current < self.n and not isinstance(self.adjList[self.current], set):
                self.current += 1
            if self.current < self.n:
                self.current += 1
                return self.current - 1
        raise StopIteration


class InEdgeIterator:
    def __init__(self, adjList, vertex):
        self.__adjList = adjList
        self.vertex = vertex
        self.current = 0

    def __iter__(self):
        return self

    def __next__(self):
        while self.current < len(self.__adjList):
            self.current += 1
            if isinstance(self.__adjList[self.current - 1], set) and self.vertex in self.__adjList[self.current - 1]:
                return self.current - 1
        raise StopIteration

class OutEdgeIterator:
    def __init__(self, adjList):
        self.__adjList = adjList
        self.current = iter(self.__adjList)

    def __iter__(self):
        return self

    def __next__(self):
        if self.current:
            return next(self.current)
        raise StopIteration

class DirectedGraph:
    def __init__(self, n):
        self.__numVertices = n
        self.__adjList = [set() for _ in range(n)]
        self.__edgeCosts = {}
        self.__inDegrees = {i: 0 for i in range(n)} 

    def addVertex(self):
        for i in range(self.__numVertices):
            if not isinstance(self.__adjList[i], set):
                self.__adjList[i] = set()
                self.__inDegrees[i] = 0
                return
        self.__adjList.append(set())
        self.__inDegrees[self.__numVertices] = 0
        self.__numVertices += 1
        

    def removeVertex(self, vertex):
        if vertex < 0 or vertex >= self.__numVertices:
            raise Exception("Invalid vertex")

        self.__adjList[vertex] = None 
        self.__inDegrees[vertex] = None

        for i in range(self.__numVertices):
            if i == vertex:
                continue
            if vertex in self.__adjList[i]:
                self.__adjList[i].remove(vertex)

        self.__edgeCosts = {k: v for k, v in self.__edgeCosts.items() if k.source != vertex and k.target != vertex}


    def addEdge(self, source, target, cost):
        self.__adjList[source].add(target)
        self.__edgeCosts[Edge_id(source, target)] = cost
        self.__inDegrees[target] += 1

    def removeEdge(self, source, target):
        self.__adjList[source].remove(target)
        del self.__edgeCosts[Edge_id(source, target)]
        self.__inDegrees[target] -= 1

    @property
    def numVertices(self):  
        count = 0
        for i in range(self.__numVertices):
            if isinstance(self.__adjList[i], set):
                count += 1
        return count

    @property
    def edgeCosts(self):
        return self.__edgeCosts

    @property
    def adjList(self):
        return self.__adjList

    def VertexIterator(self) -> 'VertexIterator':
        return VertexIterator(self.__numVertices, self.__adjList)

    def InEdgeIterator(self, vertex) -> 'InEdgeIterator':
        return InEdgeIterator(self.__adjList, vertex)

    def OutEdgeIterator(self, vertex) -> 'OutEdgeIterator':
        return OutEdgeIterator(copy.deepcopy(self.__adjList[vertex]))

    def getEdgeId(self, source, taraget) -> 'Edge_id':
        if Edge_id(source, taraget) in self.__edgeCosts:
            return Edge_id(source, taraget)
        raise Exception("Edge not found")

    def getInDegree(self, vertex) -> int:
        if not isinstance(self.__adjList[vertex], set):
            return -1
        return self.__inDegrees[vertex] 

    def getOutDegree(self, vertex) -> int:
        return len(self.__adjList[vertex])


    def getEndpoints(self, edge) -> tuple:
        return edge.source, edge.target

    def getEdgeCost(self, edge) -> int:
        return self.__edgeCosts[edge]

    def setEdgeCost(self, edge, cost):
        self.__edgeCosts[edge] = cost
    

    def writeGraphtoFile(self, filename):
        with open(filename, "w") as file:
            file.write(str(self.__numVertices) + " " + str(len(self.__edgeCosts)) + "\n")
            for i in range(self.__numVertices):
                for j in self.__adjList[i]:
                    file.write(str(i) + " " + str(j) + " " + str(self.__edgeCosts[Edge_id(i, j)]) + "\n")

    @staticmethod
    def readGraphFromFile(filename) -> 'DirectedGraph':
        with open(filename, "r") as file:
            numVertices, numEdges = file.readline().split()
            graph = DirectedGraph(int(numVertices))
            for line in file:
                source, target, cost = line.split()
                graph.addEdge(int(source), int(target), int(cost))
            return graph

    @staticmethod
    def createRandomGraph(numVertices, numEdges) -> 'DirectedGraph':
        graph = DirectedGraph(numVertices)
        for _ in range(numEdges):
            source = random.randint(0, numVertices - 1)
            target = random.randint(0, numVertices - 1)
            while Edge_id(source, target) in graph.edgeCosts:
                source = random.randint(0, numVertices - 1)
                target = random.randint(0, numVertices - 1)
            cost = random.randint(0, 100)
            graph.addEdge(source, target, cost)
        return graph

    @staticmethod
    def copyGraph(graph) -> 'DirectedGraph':
        return copy.deepcopy(graph)

def test_graph():
    graph = DirectedGraph.readGraphFromFile("graph.txt")

    # print(graph.adjList)
    # print(graph.edgeCosts)

    print("numVertices")
    print(graph.numVertices)
    print("numEdges")
    print(len(graph.edgeCosts))
    
    for i in graph.VertexIterator():
        print("vertex "+ str(i) + ":", end=" ")

        print("Out:", end=" ")
        for j in graph.OutEdgeIterator(i):
            print(j, end=" ")

        print("; In:", end=" ")
        for j in graph.InEdgeIterator(i):
            print(j, end=" ")
        print()
    

    print("getEdgeId 0 1")
    print(graph.getEdgeId(0, 1))

    print("getEdgeId: 0 2 (exception)")
    try:
        print(graph.getEdgeId(0, 2))    
    except Exception as e:
        print(e)


    print("getInDegree: 1")
    print(graph.getInDegree(1))

    print("getOutDegree: 1")
    print(graph.getOutDegree(1))

    print("getEndpoints: 0 1")
    print(graph.getEndpoints(Edge_id(0, 1)))

    print("getEdgeCost: 0 1")
    print(graph.getEdgeCost(Edge_id(0, 1)))


    print("setEdgeCost: 0 1 100")
    graph.setEdgeCost(Edge_id(0, 1), 100)
    print(graph.getEdgeCost(Edge_id(0, 1)))

    print("copyGraph")
    graphCopy = DirectedGraph.copyGraph(graph)
    print("removeVertex: 1")
    graphCopy.removeVertex(1)
    for i in graphCopy.VertexIterator():
        print(str(i) + ":", end=" ")

        print("Out:", end=" ")
        for j in graphCopy.OutEdgeIterator(i):
            print(j, end=" ")

        print("; In:", end=" ")
        for j in graphCopy.InEdgeIterator(i):
            print(j, end=" ")
        print()

    print("original graph") 
    for i in graph.VertexIterator():
        print(str(i) + ":", end=" ")

        print("Out:", end=" ")
        for j in graph.OutEdgeIterator(i):
            print(j, end=" ")

        print("; In:", end=" ")
        for j in graph.InEdgeIterator(i):
            print(j, end=" ")
        print()

############################################################################################################

def readGraphFromFile(filename):
    graph = DirectedGraph.readGraphFromFile(filename)
    return graph

def createRandomGraph(numVertices, numEdges):
    graph = DirectedGraph.createRandomGraph(numVertices, numEdges)
    return graph

def copyGraph(graph):
    return DirectedGraph.copyGraph(graph)

def writeGraphtoFile(graph, filename):
    graph.writeGraphtoFile(filename)

def printGraph(graph):
    for i in graph.VertexIterator():
        print(str(i) + ":", end=" ")

        print("Out:", end=" ")
        for j in graph.OutEdgeIterator(i):
            print(j, end=" ")

        print("; In:", end=" ")
        for j in graph.InEdgeIterator(i):
            print(j, end=" ")
        print()

def manage(graph):
    while True:
        print("1. Add vertex")
        print("2. Remove vertex")
        print("3. Add edge")
        print("4. Remove edge")
        print("5. In degree")
        print("6. Out degree")
        print("7. Endpoints")
        print("8. Edge cost")
        print("9. Set edge cost")
        print("10. Back")

        option = int(input("Option: "))
        if option == 1:
            graph.addVertex()
        elif option == 2:
            vertex = int(input("Vertex: "))
            graph.removeVertex(vertex)
        elif option == 3:
            source = int(input("Source: "))
            target = int(input("Target: "))
            cost = int(input("Cost: "))
            graph.addEdge(source, target, cost)
        elif option == 4:
            source = int(input("Source: "))
            target = int(input("Target: "))
            graph.removeEdge(source, target)
        elif option == 5:
            vertex = int(input("Vertex: "))
            print(graph.getInDegree(vertex))
            for i in graph.InEdgeIterator(vertex):
                print(i, end=" ")
            print()
        elif option == 6:
            vertex = int(input("Vertex: "))
            print(graph.getOutDegree(vertex))
            for i in graph.OutEdgeIterator(vertex):
                print(i, end=" ")
        elif option == 7:
            source = int(input("Source: "))
            target = int(input("Target: "))
            print(graph.getEndpoints(Edge_id(source, target)))
        elif option == 8:
            source = int(input("Source: "))
            target = int(input("Target: "))
            print(graph.getEdgeCost(Edge_id(source, target)))
        elif option == 9:
            source = int(input("Source: "))
            target = int(input("Target: "))
            cost = int(input("Cost: "))
            graph.setEdgeCost(Edge_id(source, target), cost)
        elif option == 10:
            break
        else:
            print("Invalid option")

def menu():
    graph = None
    copy_graph = None
    while True:
        print("1. Read graph from file")
        print("2. Create random graph")
        print("3. Copy graph")
        print("4. Write graph to file")
        print("5. Print graph")
        print("6. Manage graph")
        print("7. Number of vertices")
        print("8. Exit")

        option = int(input("Option: "))
        if option == 1:
            filename = input("Filename: ")
            graph = readGraphFromFile(filename)
        elif option == 2:
            numVertices = int(input("Number of vertices: "))
            numEdges = int(input("Number of edges: "))
            graph = createRandomGraph(numVertices, numEdges)
        elif option == 3:
            copy_graph = copyGraph(graph)
        elif option == 4:
            filename = input("Filename: ")
            writeGraphtoFile(graph, filename)
        elif option == 5:
            print("pick graph")
            print("1. Original graph")
            print("2. Copied graph")
            option = int(input("Option: "))
            if option == 1:
                printGraph(graph)
            elif option == 2:
                printGraph(copy_graph)
            else:
                print("Invalid option")
        elif option == 6:
            if graph is None:
                print("No graph")
                continue
            manage(graph)
        elif option == 7:
            if graph is None:
                print("No graph")
                continue
            print(graph.numVertices)
        elif option == 8:
            break
        else:
            print("Invalid option")



if __name__ == "__main__":
#   test_graph()
    # graph = DirectedGraph.readGraphFromFile("graph1m.txt")
    menu()