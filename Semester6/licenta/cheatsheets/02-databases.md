# Databases — Cheatsheet

Exam style: **Problem 1** — design a BCNF relational schema from a text description, marking primary / candidate / foreign keys. **Problem 2** — write an SQL query, evaluate a given query by hand on sample data, and decide whether given functional dependencies hold.

---

## 1. The relational model

- **Relation** = table. **Tuple** = row. **Attribute** = column. **Domain** = set of allowed values for an attribute.
- **Schema**: `R(A₁, A₂, …, Aₙ)` — structure. **Instance**: the current set of tuples. A relation is a **set** (no duplicate tuples, no order).
- **Degree** = number of attributes; **cardinality** = number of tuples.

### Keys
- **Superkey**: attribute set that uniquely identifies each tuple.
- **Candidate key**: a *minimal* superkey (no attribute can be removed). A relation can have several.
- **Primary key (PK)**: one chosen candidate key. Underlined in schema notation. Cannot be NULL.
- **Foreign key (FK)**: attribute(s) in one relation referencing the PK/candidate key of another (may be the same relation). Written in *italics*.

### Integrity constraints
- **Domain constraint**: each value belongs to the attribute's domain (correct type/range, e.g. `salary > 0`).
- **Key constraint**: candidate key values are unique across tuples.
- **Entity integrity**: primary key attributes cannot be NULL.
- **Referential integrity (FK constraint)**: an FK value must either match an existing PK value in the referenced relation, or be NULL.

### Notation used in exams
```
Students[IDS, CNP, LastName, FirstName]      PK underlined solid; candidate key dashed
Exams[IDS, IDC, Grade]                        {IDS} FK → Students; {IDC} FK → Courses
```

---

## 2. Designing a BCNF schema (Problem 1 recipe)

1. **Find entities** → one table each, give it a PK (code/ID).
2. **Add attributes** to the right entity; mark **non-null / unique** as stated.
3. **Translate relationships by cardinality:**
   - **1:n** → put an FK on the "many" side (referencing the "one" side's PK).
   - **n:m** → create a **junction table** whose PK = (FK₁, FK₂); attributes of the relationship go here.
   - **1:1** → FK + UNIQUE on either side.
4. **Mark candidate keys** (e.g. a unique email, a unique phone number → candidate key, dash-underlined).
5. Check each table is **BCNF**: every non-trivial FD has a superkey on the left (see §7).

Example translation: "an employee belongs to one department and one project" → `Employee` gets FKs `DeptCode`, `ProjCode`. "a location can serve many departments and a department many locations" → junction table `DeptLocation[DeptCode, LocationCode]`.

---

## 3. SQL — DDL (Data Definition Language)

### CREATE TABLE + constraints
```sql
CREATE TABLE Department (
    code  INT PRIMARY KEY,
    name  VARCHAR(50) NOT NULL UNIQUE
);

CREATE TABLE Employee (
    code     INT PRIMARY KEY,
    name     VARCHAR(50) NOT NULL,
    email    VARCHAR(80) UNIQUE,
    salary   DECIMAL(10,2) DEFAULT 0 CHECK (salary > 0),
    hireDate DATE,
    deptCode INT,
    CONSTRAINT fk_dept FOREIGN KEY (deptCode) REFERENCES Department(code)
        ON DELETE SET NULL ON UPDATE CASCADE
);
```
| Constraint | Effect |
|-----------|--------|
| `PRIMARY KEY` | unique + not null, one per table |
| `FOREIGN KEY … REFERENCES` | referential integrity |
| `UNIQUE` | no duplicate values (NULLs allowed) |
| `CHECK (cond)` | domain/business rule |
| `NOT NULL` | value required |
| `DEFAULT v` | value used if none supplied |

FK actions: `ON DELETE/UPDATE` → `CASCADE`, `SET NULL`, `SET DEFAULT`, `NO ACTION`/`RESTRICT`.

### ALTER / DROP
```sql
ALTER TABLE Employee ADD phone VARCHAR(20);
ALTER TABLE Employee DROP COLUMN phone;
ALTER TABLE Employee ADD CONSTRAINT chk CHECK (salary < 100000);
DROP TABLE Employee;          -- removes table + data
```

---

## 4. SQL — DML (Data Manipulation Language)

```sql
INSERT INTO Department (code, name) VALUES (1, 'IT');
INSERT INTO Department VALUES (2, 'HR');           -- all columns, in order

UPDATE Employee SET salary = salary * 1.1 WHERE deptCode = 1;

DELETE FROM Employee WHERE salary IS NULL;
```

---

## 5. Three-valued logic (NULL)

NULL = unknown/missing. Comparisons with NULL yield **UNKNOWN** (not TRUE/FALSE).

- `x = NULL` → UNKNOWN. Use **`IS NULL`** / **`IS NOT NULL`**.
- A `WHERE` clause keeps a row only when the condition is **TRUE** (UNKNOWN rows are dropped).

| AND | T | F | U |   | OR | T | F | U |   | NOT | |
|-----|---|---|---|---|----|---|---|---|---|-----|--|
| **T** | T | F | U |   | **T** | T | T | T |   | T | F |
| **F** | F | F | F |   | **F** | T | F | U |   | F | T |
| **U** | U | F | U |   | **U** | T | U | U |   | U | U |

Aggregates **ignore NULLs** (except `COUNT(*)` which counts rows). `COUNT(col)` skips NULLs.

---

## 6. SQL — SELECT

### Clause syntax order
```sql
SELECT [DISTINCT | TOP n] columns / aggregates
FROM tables / joins
WHERE row_condition
GROUP BY columns
HAVING group_condition
ORDER BY columns [ASC|DESC];
```

### Logical evaluation order (important for tracing)
`FROM` → `WHERE` → `GROUP BY` → `HAVING` → `SELECT` → `DISTINCT` → `ORDER BY` → `TOP`.
> Consequence: column **aliases** from SELECT can be used in ORDER BY but not in WHERE; `WHERE` filters rows, `HAVING` filters groups.

### Filtering operators
```sql
WHERE price BETWEEN 10 AND 50          -- inclusive
WHERE name LIKE 'A%'                    -- % = any chars, _ = one char
WHERE deptCode IN (1, 2, 3)
WHERE deptCode IN (SELECT code FROM Department WHERE name='IT')
```

### Joins
```sql
FROM A INNER JOIN B ON A.k = B.k        -- matching rows only
FROM A LEFT  JOIN B ON A.k = B.k        -- all A, NULLs where no B
FROM A RIGHT JOIN B ON A.k = B.k        -- all B, NULLs where no A
FROM A FULL  JOIN B ON A.k = B.k        -- all rows from both
```

### Aggregates + grouping
```sql
SELECT deptCode, COUNT(*) AS cnt, AVG(salary) AS avgSal,
       MIN(salary), MAX(salary), SUM(salary)
FROM Employee
GROUP BY deptCode
HAVING COUNT(*) > 5;                     -- condition on groups
```
- Non-aggregated SELECT columns **must** appear in `GROUP BY`.
- `COUNT(*)` counts rows; `COUNT(col)` skips NULL; `COUNT(DISTINCT col)` counts distinct non-NULL.

### Set operators (combine two SELECTs with matching columns)
```sql
SELECT ... UNION     SELECT ...    -- union, duplicates removed
SELECT ... UNION ALL SELECT ...    -- union, duplicates kept
SELECT ... INTERSECT SELECT ...    -- rows in both
SELECT ... EXCEPT    SELECT ...    -- rows in first, not in second
```

### Subqueries (nested queries)
```sql
-- scalar subquery
WHERE salary > (SELECT AVG(salary) FROM Employee)

-- IN
WHERE deptCode IN (SELECT code FROM Department WHERE name='IT')

-- EXISTS (correlated: references outer query)
WHERE EXISTS (SELECT 1 FROM Orders o WHERE o.empCode = e.code)

-- ANY / ALL
WHERE salary > ANY (SELECT salary FROM Employee WHERE deptCode=2)  -- > at least one (> min)
WHERE salary > ALL (SELECT salary FROM Employee WHERE deptCode=2)  -- > every one  (> max)
```
- `= ANY` ≡ `IN`. `<> ALL` ≡ `NOT IN`.
- **Correlated** subquery: re-evaluated per outer row (references outer table). **EXISTS** stops at the first match.

### TOP / DISTINCT
```sql
SELECT DISTINCT field FROM Books;        -- remove duplicate rows
SELECT TOP 3 * FROM Employee ORDER BY salary DESC;   -- first 3 (SQL Server); MySQL/Postgres use LIMIT
```

### Worked pattern — "authors with ≥2 books in one year AND a book by publisher DE2"
```sql
SELECT b.AuthorID, a.Name
FROM Books b JOIN Authors a ON b.AuthorID = a.AuthorID
WHERE b.AuthorID IN (                         -- has a book via publisher DE2
    SELECT bp.BookID FROM BooksPublishers bp
    JOIN Publishers p ON bp.PublisherID = p.PublisherID
    WHERE p.Name = 'DE2')                       -- (simplified; align keys per schema)
GROUP BY b.AuthorID, a.Name, b.YearWritten
HAVING COUNT(*) >= 2;
```

### Evaluating a query by hand (Problem 2 b1)
1. Resolve `FROM`/joins → working set of rows.
2. Apply `WHERE` (drop UNKNOWN).
3. `GROUP BY` → buckets; compute aggregates.
4. Apply `HAVING`.
5. Project `SELECT` columns; apply `DISTINCT`, `ORDER BY`, `TOP`.
6. Report only the **column names** and **tuple values** of the result.

---

## 7. Functional dependencies & normal forms

### Functional dependency (FD)
`X → Y`: any two tuples that agree on X must agree on Y ("X determines Y").
- **Checking against an instance**: it holds if no two rows have same X but different Y. One counterexample ⇒ does **not** hold. (An FD can be *satisfied by the data* yet not be a real schema rule.)

### Armstrong's axioms (sound & complete)
1. **Reflexivity**: if `Y ⊆ X` then `X → Y` (trivial FD).
2. **Augmentation**: if `X → Y` then `XZ → YZ`.
3. **Transitivity**: if `X → Y` and `Y → Z` then `X → Z`.

Derived rules:
4. **Union**: if `X → Y` and `X → Z` then `X → YZ`.
5. **Decomposition**: if `X → YZ` then `X → Y` and `X → Z`.
6. **Pseudotransitivity**: if `X → Y` and `WY → Z` then `WX → Z`.

### Attribute closure  X⁺
Set of all attributes determined by X. Algorithm: start with `X⁺ = X`; repeatedly, for each FD `A → B` with `A ⊆ X⁺`, add `B`; until stable.
- **X is a candidate key** ⇔ `X⁺ = all attributes` and X is minimal.

### Normal forms (each implies the previous)
| NF | Requirement |
|----|-------------|
| **1NF** | atomic values only — no repeating groups / multivalued cells |
| **2NF** | 1NF **and** no non-prime attribute depends on *part* of a candidate key (no **partial** dependency). Relevant only with composite keys. |
| **3NF** | 2NF **and** no non-prime attribute depends on another non-prime attribute (no **transitive** dependency). For each FD `X→A`: X is a superkey, **or** A is prime. |
| **BCNF** | for **every** non-trivial FD `X→Y`, X is a **superkey**. Stricter than 3NF. |

- *Prime attribute* = part of some candidate key.
- Goal of decomposition: eliminate redundancy/anomalies while keeping it lossless (and ideally dependency-preserving).
- **2NF violation** example: `R(StudentID, CourseID, StudentName)` with key `{StudentID, CourseID}` — `StudentID → StudentName` is partial. Fix: split out `Student(StudentID, StudentName)`.
- **3NF violation** example: `R(EmpID, DeptID, DeptName)`, `EmpID→DeptID→DeptName` transitive. Fix: split out `Dept(DeptID, DeptName)`.

---

## 8. Relational algebra (on sets)

Operators consume relations and produce relations. Notation summary:

| Operation | Symbol | Meaning |
|-----------|--------|---------|
| **Selection** | σ_cond(R) | rows of R satisfying *cond* |
| **Projection** | π_A,B(R) | keep columns A,B (set → dups removed) |
| **Cross-product** | R × S | every row of R paired with every row of S |
| **Union** | R ∪ S | rows in R or S (union-compatible) |
| **Set-difference** | R − S | rows in R but not in S |
| **Intersection** | R ∩ S | rows in both |
| **Condition (theta) join** | R ⋈_θ S | σ_θ(R × S) |
| **Natural join** | R ⋈ S | join on all common attributes, common columns merged |
| **Left outer join** | R ⟕ S | all R rows; unmatched S side → NULL |
| **Right outer join** | R ⟖ S | all S rows; unmatched R side → NULL |
| **Full outer join** | R ⟗ S | all rows of both; missing side → NULL |
| **Division** | R ÷ S | tuples in R associated with **all** tuples of S |
| **Assignment** | T ← expr | name an intermediate result |
| **Rename** | ρ_x(R) | rename relation/attributes |

- **Union / intersection / difference** require **union-compatibility**: same number of attributes, matching domains.
- **σ** picks rows (horizontal), **π** picks columns (vertical).
- **Natural join** = cross-product, keep rows equal on common attributes, drop duplicate columns. If no common attributes ⇒ equals cross-product.
- **Division** answers "for all" queries — e.g. *students enrolled in every course*: `Enrolled ÷ Courses`. Result schema = attributes of R not in S.

### Examples
```
σ_salary>5000 (Employee)                       -- high earners
π_name,salary (Employee)                        -- two columns
Employee ⋈ Department                           -- natural join on deptCode
σ_E.dept=D.code (Employee × Department)         -- equivalent theta join
π_empName (Employee ⟕ Department)               -- include employees with no dept (NULL)
Assigned(emp, proj) ÷ AllProjects(proj)         -- employees on all projects
```

### SQL ↔ relational algebra
| SQL | Algebra |
|-----|---------|
| `WHERE` | σ |
| `SELECT cols` | π |
| `INNER JOIN ON` | ⋈_θ |
| `,` in FROM | × |
| `UNION` | ∪ |
| `INTERSECT` | ∩ |
| `EXCEPT` | − |
