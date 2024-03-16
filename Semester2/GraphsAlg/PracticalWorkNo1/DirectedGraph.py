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
    def __init__(self, n):
        self.n = n
        self.current = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.current < self.n:
            self.current += 1
            return self.current - 1
        raise StopIteration

class InEdgeIterator:
    def __init__(self, adjList, vertex):
        self.adjList = adjList
        self.vertex = vertex
        self.current = 0

    def __iter__(self):
        return self

    def __next__(self):
        while self.current < len(self.adjList):
            self.current += 1
            if self.vertex in self.adjList[self.current - 1]:
                return self.current - 1
        raise StopIteration

class OutEdgeIterator:
    def __init__(self, adjList):
        self.adjList = adjList
        self.current = iter(self.adjList)

    def __iter__(self):
        return self

    def __next__(self):
        if self.current:
            return next(self.current)
        raise StopIteration

class DirectedGraph:
    def __init__(self, n):
        self.numVertices = n
        self.adjList = [set() for _ in range(n)] 
        self.edgeCosts = {}

    def addEdge(self, source, target, cost):
        self.adjList[source].add(target)
        self.edgeCosts[Edge_id(source, target)] = cost

    def removeEdge(self, source, target):
        self.adjList[source].remove(target)
        del self.edgeCosts[Edge_id(source, target)]

    def VertexIterator(self) -> 'VertexIterator':
        return VertexIterator(self.numVertices)

    def InEdgeIterator(self, vertex) -> 'InEdgeIterator':
        return InEdgeIterator(self.adjList, vertex)

    def OutEdgeIterator(self, vertex) -> 'OutEdgeIterator':
        return OutEdgeIterator(copy.deepcopy(self.adjList[vertex]))

    def getEdgeId(self, source, taraget) -> 'Edge_id':
        if Edge_id(source, taraget) in self.edgeCosts:
            return Edge_id(source, taraget)
        raise Exception("Edge not found")

    def getInDegree(self, vertex) -> int:
        k = 0
        for i in range(self.numVertices):
            if vertex in self.adjList[i]:
                k += 1
        return k 

    def getOutDegree(self, vertex) -> int:
        return len(self.adjList[vertex])


    def getEndpoints(self, edge) -> tuple:
        return edge.source, edge.target

    def getEdgeCost(self, edge) -> int:
        return self.edgeCosts[edge]

    def setEdgeCost(self, edge, cost):
        self.edgeCosts[edge] = cost
    

    def writeGraphtoFile(self, filename):
        with open(filename, "w") as file:
            file.write(str(self.numVertices) + " " + str(len(self.edgeCosts)) + "\n")
            for i in range(self.numVertices):
                for j in self.adjList[i]:
                    file.write(str(i) + " " + str(j) + " " + str(self.edgeCosts[Edge_id(i, j)]) + "\n")

    @staticmethod
    def readGraphFromFile(filename) -> 'DirectedGraph':
        with open(filename, "r") as file:
            numVertices, numEdges = file.readline().split()
            graph = DirectedGraph(int(numVertices))
            next(file)
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
        newGraph = DirectedGraph(graph.numVertices)
        newGraph.adjList = copy.deepcopy(graph.adjList)
        newGraph.edgeCosts = copy.deepcopy(graph.edgeCosts)
        return newGraph


if __name__ == "__main__":
    graph = DirectedGraph.readGraphFromFile("graph.txt")
    print(graph.adjList)
    print(graph.edgeCosts)
    for i in graph.VertexIterator():
        print(str(i) + ":", end=" ")

        print("Out:", end=" ")
        for j in graph.OutEdgeIterator(i):
            print(j, end=" ")

        print("; In:", end=" ")
        for j in graph.InEdgeIterator(i):
            print(j, end=" ")
        print()
    
    print(graph.getEdgeId(0, 1))
    try:
        print(graph.getEdgeId(0, 2))    
    except Exception as e:
        print(e)