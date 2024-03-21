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
        self.__adjList = adjList
        self.vertex = vertex
        self.current = 0

    def __iter__(self):
        return self

    def __next__(self):
        while self.current < len(self.__adjList):
            self.current += 1
            if self.vertex in self.__adjList[self.current - 1]:
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

    def addVertex(self):
        self.__numVertices += 1
        self.__adjList.append(set())

    def removeVertex(self, vertex):
        if vertex < 0 or vertex >= self.__numVertices:
            raise Exception("Invalid vertex")

        self.__adjList[vertex] = set() 

        for i in range(self.__numVertices):
            if vertex in self.__adjList[i] and i != vertex:
                self.__adjList[i].remove(vertex)

        self.__edgeCosts = {k: v for k, v in self.__edgeCosts.items() if k.source != vertex and k.target != vertex}


    def addEdge(self, source, target, cost):
        self.__adjList[source].add(target)
        self.__edgeCosts[Edge_id(source, target)] = cost

    def removeEdge(self, source, target):
        self.__adjList[source].remove(target)
        del self.__edgeCosts[Edge_id(source, target)]

    @property
    def numVertices(self):
        return self.__numVertices

    @property
    def edgeCosts(self):
        return self.__edgeCosts

    @property
    def adjList(self):
        return self.__adjList

    def VertexIterator(self) -> 'VertexIterator':
        return VertexIterator(self.__numVertices)

    def InEdgeIterator(self, vertex) -> 'InEdgeIterator':
        return InEdgeIterator(self.__adjList, vertex)

    def OutEdgeIterator(self, vertex) -> 'OutEdgeIterator':
        return OutEdgeIterator(copy.deepcopy(self.__adjList[vertex]))

    def getEdgeId(self, source, taraget) -> 'Edge_id':
        if Edge_id(source, taraget) in self.__edgeCosts:
            return Edge_id(source, taraget)
        raise Exception("Edge not found")

    def getInDegree(self, vertex) -> int:
        k = 0
        for i in range(self.__numVertices):
            if vertex in self.__adjList[i]:
                k += 1
        return k 

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


if __name__ == "__main__":
    graph = DirectedGraph.readGraphFromFile("graph1m.txt")

    # print(graph.adjList)
    # print(graph.edgeCosts)

    print("numVertices")
    print(graph.numVertices)
    print("numEdges")
    print(len(graph.edgeCosts))
    
    # for i in graph.VertexIterator():
        # print(str(i) + ":", end=" ")

        # print("Out:", end=" ")
        # for j in graph.OutEdgeIterator(i):
        #     print(j, end=" ")

        # print("; In:", end=" ")
        # for j in graph.InEdgeIterator(i):
        #     print(j, end=" ")
        # print()
    

    # print("getEdgeId")
    # print(graph.getEdgeId(0, 1))
    # try:
    #     print(graph.getEdgeId(0, 2))    
    # except Exception as e:
    #     print(e)


    # print("getInDegree: 1")
    # print(graph.getInDegree(1))

    # print("getOutDegree: 1")
    # print(graph.getOutDegree(1))

    # print("getEndpoints: 0 1")
    # print(graph.getEndpoints(Edge_id(0, 1)))

    # print("getEdgeCost: 0 1")
    # print(graph.getEdgeCost(Edge_id(0, 1)))


    # print("setEdgeCost: 0 1 100")
    # graph.setEdgeCost(Edge_id(0, 1), 100)
    # print(graph.getEdgeCost(Edge_id(0, 1)))

    # print("copyGraph")
    # graphCopy = DirectedGraph.copyGraph(graph)
    # print("removeVertex: 1")
    # graphCopy.removeVertex(1)
    # for i in graphCopy.VertexIterator():
    #     print(str(i) + ":", end=" ")

    #     print("Out:", end=" ")
    #     for j in graphCopy.OutEdgeIterator(i):
    #         print(j, end=" ")

    #     print("; In:", end=" ")
    #     for j in graphCopy.InEdgeIterator(i):
    #         print(j, end=" ")
    #     print()

    # print("original graph") 
    # for i in graph.VertexIterator():
    #     print(str(i) + ":", end=" ")

    #     print("Out:", end=" ")
    #     for j in graph.OutEdgeIterator(i):
    #         print(j, end=" ")

    #     print("; In:", end=" ")
    #     for j in graph.InEdgeIterator(i):
    #         print(j, end=" ")
    #     print()

