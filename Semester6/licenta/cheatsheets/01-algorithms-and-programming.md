# Algorithms & Programming — Cheatsheet

Courses: *Programming fundamentals, OOP, Data structures and algorithms*

Exam style: write a **full** function/class in Python, C++, Java or C# (or pseudocode); state best/average/worst complexity with justification; implement classes from a UML diagram; pick the efficient data structure.

---

## 1. Algorithm complexity

### Asymptotic notation
Describes how running time / memory grows with input size `n`.

| Notation | Meaning | Bound |
|----------|---------|-------|
| `O(f)` | grows **at most** as fast as f | upper bound (worst) |
| `Ω(f)` | grows **at least** as fast as f | lower bound (best) |
| `Θ(f)` | grows **exactly** as fast as f | tight bound (`O` and `Ω`) |

`T(n) = Θ(f(n))` ⇔ ∃ c₁,c₂>0, n₀ such that `c₁·f(n) ≤ T(n) ≤ c₂·f(n)` for all `n ≥ n₀`.

### Cases
- **Best case** (`Ω`): most favorable input (e.g. already found at position 0).
- **Worst case** (`O`): least favorable input (e.g. element absent).
- **Average case** (`Θ`): expected over all inputs (assume uniform distribution unless told otherwise).

### Complexity classes (slow → fast growth)
`O(1) < O(log n) < O(n) < O(n log n) < O(n²) < O(n³) < O(2ⁿ) < O(n!)`

### How to analyze
- Sequential statements → add (dominant term wins).
- Nested loops → multiply (two loops to n → `n²`).
- Loop that halves the range each step → `log n`.
- Recursion → write a recurrence, then solve.

**Master theorem** for `T(n) = a·T(n/b) + Θ(nᵈ)`:
- if `d > log_b a` → `Θ(nᵈ)`
- if `d = log_b a` → `Θ(nᵈ log n)`
- if `d < log_b a` → `Θ(n^(log_b a))`

Examples: merge sort `T(n)=2T(n/2)+Θ(n)` → `Θ(n log n)`; binary search `T(n)=T(n/2)+Θ(1)` → `Θ(log n)`.

---

## 2. Pseudocode conventions

```
Subalgorithm name(params):
    // assignment
    x ← 0
    // selection
    If cond Then ... Else ... EndIf
    // loops
    For i ← 1, n execute ... EndFor
    While cond execute ... EndWhile
    // output / return
    return value
End
```

A clear, unambiguous, language-independent description of the steps. State pre/post-conditions when relevant.

---

## 3. Searching

### Sequential (linear) search — works on unsorted data
```python
def seq_search(a, x):
    for i in range(len(a)):
        if a[i] == x:
            return i          # found
    return -1                 # not found
```
- Best `Θ(1)` (first position) · Average `Θ(n)` · Worst `Θ(n)`.

### Binary search — requires the array to be **sorted**
```python
def binary_search(a, x):          # a sorted ascending
    lo, hi = 0, len(a) - 1
    while lo <= hi:
        mid = (lo + hi) // 2
        if a[mid] == x:
            return mid
        elif a[mid] < x:
            lo = mid + 1
        else:
            hi = mid - 1
    return -1
```
- Best `Θ(1)` · Average `Θ(log n)` · Worst `Θ(log n)`.
- Recursive form: `T(n) = T(n/2) + Θ(1)`.

---

## 4. Merging two sorted lists

Core of merge sort; also a standalone exam task ("interclasare").
```python
def merge(a, b):                  # a, b sorted ascending → result sorted
    i = j = 0
    res = []
    while i < len(a) and j < len(b):
        if a[i] <= b[j]:
            res.append(a[i]); i += 1
        else:
            res.append(b[j]); j += 1
    res.extend(a[i:]); res.extend(b[j:])   # leftovers
    return res
```
Complexity `Θ(n + m)`. Stable if you use `<=`.

**Recursive merge** (common exam requirement):
```python
def merge_rec(a, b):
    if not a: return b[:]
    if not b: return a[:]
    if a[0] <= b[0]:
        return [a[0]] + merge_rec(a[1:], b)
    else:
        return [b[0]] + merge_rec(a, b[1:])
```

---

## 5. Sorting

### Selection sort — pick the minimum, swap into place
```python
def selection_sort(a):
    for i in range(len(a)):
        m = i
        for j in range(i + 1, len(a)):
            if a[j] < a[m]:
                m = j
        a[i], a[m] = a[m], a[i]
```
All cases `Θ(n²)`. In-place. **Not stable**. Few swaps (`n`).

### Bubble sort — repeatedly swap adjacent out-of-order pairs
```python
def bubble_sort(a):
    n = len(a)
    for i in range(n - 1):
        swapped = False
        for j in range(n - 1 - i):
            if a[j] > a[j + 1]:
                a[j], a[j + 1] = a[j + 1], a[j]
                swapped = True
        if not swapped:            # already sorted
            break
```
Best `Θ(n)` (with the flag, already sorted) · Avg/Worst `Θ(n²)`. In-place. **Stable**.

### Insertion sort — insert each element into the sorted prefix
```python
def insertion_sort(a):
    for i in range(1, len(a)):
        key, j = a[i], i - 1
        while j >= 0 and a[j] > key:
            a[j + 1] = a[j]
            j -= 1
        a[j + 1] = key
```
Best `Θ(n)` (sorted) · Avg/Worst `Θ(n²)`. In-place. **Stable**. Fast on small/nearly-sorted data.

### Merge sort — divide, sort halves, merge
```python
def merge_sort(a):
    if len(a) <= 1:
        return a
    mid = len(a) // 2
    left = merge_sort(a[:mid])
    right = merge_sort(a[mid:])
    return merge(left, right)      # see §4
```
All cases `Θ(n log n)`. **Stable**. Needs `Θ(n)` extra memory (not in-place).

### Quicksort — partition around a pivot, recurse
```python
def quicksort(a, lo, hi):
    if lo < hi:
        p = partition(a, lo, hi)
        quicksort(a, lo, p - 1)
        quicksort(a, p + 1, hi)

def partition(a, lo, hi):         # Lomuto, pivot = last element
    pivot = a[hi]
    i = lo - 1
    for j in range(lo, hi):
        if a[j] <= pivot:
            i += 1
            a[i], a[j] = a[j], a[i]
    a[i + 1], a[hi] = a[hi], a[i + 1]
    return i + 1
```
Best/Avg `Θ(n log n)` · Worst `Θ(n²)` (already sorted with naive pivot). In-place. **Not stable**. Usually fastest in practice.

### Comparison table
| Algorithm | Best | Average | Worst | Memory | Stable |
|-----------|------|---------|-------|--------|--------|
| Selection | n² | n² | n² | O(1) | no |
| Bubble | n | n² | n² | O(1) | yes |
| Insertion | n | n² | n² | O(1) | yes |
| Merge | n log n | n log n | n log n | O(n) | yes |
| Quick | n log n | n log n | n² | O(log n) | no |

**Stable** = equal elements keep their original relative order.

---

## 6. Backtracking

Systematically build a solution one component at a time; abandon ("backtrack") a partial candidate as soon as it cannot lead to a valid solution.

General template:
```python
def backtrack(sol, step):
    if is_solution(sol, step):
        process(sol)
        return
    for value in candidates(sol, step):
        if valid(sol, step, value):   # pruning
            sol[step] = value
            backtrack(sol, step + 1)
            # implicit undo: overwritten next iteration
```

Example — **permutations** of `1..n`:
```python
def permutations(n):
    sol, used, res = [0]*n, [False]*(n+1), []
    def bt(k):
        if k == n:
            res.append(sol[:]); return
        for v in range(1, n + 1):
            if not used[v]:
                used[v] = True; sol[k] = v
                bt(k + 1)
                used[v] = False        # undo
    bt(0)
    return res
```
Classic problems: permutations, combinations, subsets, N-queens, maze paths, sum of subset. Complexity is typically exponential — explores the solution tree.

---

## 7. OOP concepts

### Class vs object
- **Class**: blueprint/type — defines attributes (state) + methods (behavior).
- **Object**: a concrete instance of a class, with its own attribute values.

### Members & access modifiers
| Modifier | Visible from |
|----------|--------------|
| `public` | anywhere |
| `protected` | same class + subclasses |
| `private` | same class only |
| `package`/default (Java) | same package |

Convention: keep attributes private, expose via public getters/setters (**encapsulation**).

| | Python | C++ | Java | C# |
|--|--------|-----|------|----|
| public | default (`x`) | `public:` | `public` | `public` |
| protected | `_x` (convention) | `protected:` | `protected` | `protected` |
| private | `__x` (name-mangled) | `private:` (default in class) | `private` | `private` (default) |

### Constructors & destructors
Constructor: initializes a new object. Destructor: cleanup when object is destroyed.

```python
# Python
class Point:
    def __init__(self, x, y):   # constructor
        self.x = x; self.y = y
    def __del__(self):          # destructor (rarely used; GC managed)
        pass
```
```cpp
// C++
class Point {
    int x, y;
public:
    Point(int x, int y) : x(x), y(y) {}   // constructor
    ~Point() {}                           // destructor (called on delete/scope end)
};
```
```java
// Java — no destructors; GC handles memory. Constructor only:
class Point {
    private int x, y;
    public Point(int x, int y) { this.x = x; this.y = y; }
}
```
```csharp
// C#
class Point {
    private int x, y;
    public Point(int x, int y) { this.x = x; this.y = y; }
    ~Point() { }    // finalizer (GC); prefer IDisposable/Dispose()
}
```
Notes: C++ has real deterministic destructors. Java/C#/Python use garbage collection (no manual `delete`); their "destructor" is a finalizer that runs nondeterministically.

---

## 8. Inheritance & polymorphism

### Inheritance — a derived class reuses/extends a base class ("is-a")
```python
class Animal:
    def __init__(self, name): self.name = name
    def sound(self): return "..."

class Dog(Animal):                  # Dog inherits Animal
    def sound(self): return "Woof"  # override
```
```java
class Animal { String sound() { return "..."; } }
class Dog extends Animal {
    @Override String sound() { return "Woof"; }
}
```
```cpp
class Animal { public: virtual string sound() { return "..."; } };
class Dog : public Animal { public: string sound() override { return "Woof"; } };
```
```csharp
class Animal { public virtual string Sound() => "..."; }
class Dog : Animal { public override string Sound() => "Woof"; }
```

### Method overriding vs overloading
- **Overriding**: subclass redefines a base method with the same signature.
- **Overloading**: same method name, different parameter lists (same class).

### Polymorphism & dynamic binding
- **Polymorphism**: one interface, many implementations — a base-type reference can point to any derived object.
- **Dynamic (late) binding**: the actual method called is decided at **runtime** based on the object's real type.

```python
animals = [Dog(), Cat()]
for a in animals:
    print(a.sound())     # calls the right override per object
```
In C++ you must mark methods `virtual` for dynamic binding; Java/C#/Python bind dynamically by default (C# needs `virtual`/`override`).

### Abstract classes vs interfaces
- **Abstract class**: cannot be instantiated; may have abstract methods (no body) + concrete methods + state. A subclass must implement all abstract methods.
- **Interface**: pure contract — method signatures only (classically no state). A class can implement many interfaces.

| | abstract class | interface |
|--|---------------|-----------|
| instantiate | no | no |
| state/fields | yes | no (constants only, classically) |
| method bodies | yes (some) | no (classically) |
| multiple inheritance | single base | implement many |

```python
from abc import ABC, abstractmethod
class Shape(ABC):
    @abstractmethod
    def area(self): ...
class Circle(Shape):
    def __init__(self, r): self.r = r
    def area(self): return 3.14159 * self.r * self.r
```
```java
abstract class Shape { abstract double area(); }
interface Drawable { void draw(); }
class Circle extends Shape implements Drawable {
    double r; Circle(double r){this.r=r;}
    double area(){ return Math.PI*r*r; }
    public void draw(){}
}
```

---

## 9. UML class diagrams

### Class box
```
┌─────────────────────┐
│      ClassName       │
├─────────────────────┤
│ - privateAttr: int   │   visibility: + public  - private  # protected
│ + publicAttr: String │
├─────────────────────┤
│ + method(p: T): R    │
│ - helper(): void     │
└─────────────────────┘
```
Underlined member = `static`. Italic class/method = `abstract`.

### Relations between classes
| Relation | Symbol | Meaning |
|----------|--------|---------|
| **Association** | plain line `──` | uses / knows another class |
| **Aggregation** | hollow diamond `◇──` | "has-a", part can exist independently (weak) |
| **Composition** | filled diamond `◆──` | "owns-a", part dies with whole (strong) |
| **Inheritance / generalization** | hollow triangle `──▷` | "is-a" (extends) |
| **Realization** | dashed + hollow triangle `╌╌▷` | implements an interface |
| **Dependency** | dashed arrow `╌╌>` | temporary use (e.g. param/local) |

**Multiplicity** on association ends: `1`, `0..1`, `*` (many), `1..*`, `0..*`.

Example (from exam): `Company ◇── Department` (aggregation: company has a list of departments), `PartTimeEmployee ──▷ Employee` (inheritance), `Department ── Employee` (association, holds employees).

---

## 10. Data structures

### Lists
- **Array list** (dynamic array): contiguous; index access `O(1)`; insert/delete at end amortized `O(1)`, in middle `O(n)`.
- **Linked list**: nodes with pointers; insert/delete at a known position `O(1)`; index access `O(n)`. Singly vs doubly linked.

| Op | Array list | Linked list |
|----|-----------|-------------|
| access by index | O(1) | O(n) |
| insert/delete at end | O(1)* | O(1) |
| insert/delete at front | O(n) | O(1) |
| search | O(n) | O(n) |

### Maps (dictionaries / associative arrays)
Store key → value pairs, unique keys. Backed by a hash table (avg `O(1)`) or a balanced/search tree (`O(log n)`, keys ordered).

### Binary Search Tree (BST)
Each node: key, left subtree (keys `<`), right subtree (keys `>`). In-order traversal yields sorted order.
```
        8
       / \
      3   10
     / \    \
    1   6    14
```
| Op | Average | Worst (degenerate/unbalanced) |
|----|---------|------|
| search / insert / delete | O(log n) | O(n) |

- **Insert**: go left/right by comparison until an empty spot.
- **Delete**: leaf → remove; one child → replace with child; two children → replace with in-order successor (min of right subtree).
- Worst case `O(n)` when the tree becomes a "chain" (e.g. inserting already-sorted keys). *(Balanced trees are out of scope.)*

### Hash tables
Array of buckets + hash function `h(key) → index`. Average `O(1)` insert/search/delete.
- **Collisions** (two keys → same bucket):
  - **Chaining**: each bucket is a linked list.
  - **Open addressing**: probe for next free slot (linear/quadratic/double hashing).
- **Load factor** α = entries / buckets; resize (rehash) when too high.
- Worst case `O(n)` if all keys collide.

| Op | Average | Worst |
|----|---------|-------|
| insert / search / delete | O(1) | O(n) |

---

## 11. Choosing the right structure + library equivalents

### Decision guide
- Need **index access / ordered sequence** → list / array.
- Need **key→value lookup**, fast, order irrelevant → hash map.
- Need **keys kept sorted** / range queries → BST-based (tree) map.
- Need **uniqueness / membership test** → set (hash set).
- Need **fast min/max repeatedly** → heap/priority queue.
- Need **FIFO** → queue; **LIFO** → stack.

### Library types per language
| Concept | Python | Java | C++ | C# |
|---------|--------|------|-----|----|
| dynamic list | `list` | `ArrayList` | `vector` | `List<T>` |
| linked list | `collections.deque` | `LinkedList` | `list` | `LinkedList<T>` |
| hash map | `dict` | `HashMap` | `unordered_map` | `Dictionary<K,V>` |
| tree/sorted map | — (`sortedcontainers`) | `TreeMap` | `map` | `SortedDictionary<K,V>` |
| hash set | `set` | `HashSet` | `unordered_set` | `HashSet<T>` |
| tree/sorted set | — | `TreeSet` | `set` | `SortedSet<T>` |
| stack | `list` / `deque` | `Deque`/`Stack` | `stack` | `Stack<T>` |
| queue | `collections.deque` | `Queue`/`ArrayDeque` | `queue` | `Queue<T>` |
| priority queue | `heapq` | `PriorityQueue` | `priority_queue` | `PriorityQueue<T>` |

> Exam note: when complexity is required (e.g. "addEmployee must be `Θ(1)`"), justify it by the chosen structure (append to list / hash insert = `Θ(1)`; searching a hash set = `Θ(1)` avg to count distinct values, vs `Θ(n)` naive scan).
