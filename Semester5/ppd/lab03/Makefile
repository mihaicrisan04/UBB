# Java Makefile for Matrix Multiplication Lab
JAVAC = javac
JAVA = java
SOURCES = MatrixMultiply.java Benchmark.java
CLASSES = $(SOURCES:.java=.class)

# Default values (override with: make run THREADS=8)
N ?= 1000
M ?= 1000
P ?= 1000
THREADS ?= 4

all: $(CLASSES)

%.class: %.java
	$(JAVAC) $<

run: MatrixMultiply.class
	$(JAVA) MatrixMultiply $(N) $(M) $(P) $(THREADS)

benchmark: Benchmark.class MatrixMultiply.class
	$(JAVA) Benchmark

debug: MatrixMultiply.class
	$(JAVA) MatrixMultiply 9 9 9 4 debug

clean:
	rm -f *.class

.PHONY: all run benchmark debug clean