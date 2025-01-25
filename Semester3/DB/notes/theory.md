
# Theory

## SQL Query Order of Execution

The general order of SQL statement execution, from first to last, is as follows:

1. **FROM**: Specifies the tables or views to be queried.
2. **JOIN**: Defines how to combine multiple tables (if applicable).
3. **WHERE**: Filters rows before any grouping or aggregation.
4. **GROUP BY**: Groups the rows based on specified columns.
5. **HAVING**: Filters groups after the GROUP BY step.
6. **SELECT**: Determines which columns to include in the result.
7. **DISTINCT**: Removes duplicate rows from the result set.
8. **ORDER BY**: Sorts the result based on specified columns.

## Understanding B-tree order and properties

A **B-tree of order* $m$ has the following properties:
**1. Internal (non-leaf) nodes:**
    - Can have a maximum of $m$ children and $m - 1$ keys.
    - Can have a minimum of $\lceil m/2 \rceil$ children.
**2. Leaf nodes:**
    - Must all appear at the same level (balanced property).

---

## Define the key from the relational model

A key in the relational model is an attribute or a set of attributes that uniquely identifies a tuple within a relation, ensuring data integrity and uniqueness.

Types of Keys:

1. Candidate Key: A minimal set of attributes that can uniquely identify a tuple.
2. Primary Key: A chosen candidate key used to uniquely identify tuples.
3. Alternate Key: Candidate keys that are not chosen as the primary key.
4. Foreign Key: An attribute that establishes a relationship between two tables.
5. Composite Key: A key consisting of two or more attributes.

---

## Indexes in databases

### Clustered Index

A **clustered index** is a type of database index that determines the physical order of data rows in a table based on the indexed column(s).

**Key Characteristics of Clustered Indexes:**

1. **Sorting:** Data is physically stored in sorted order based on the indexed column.
2. **One per Table:** A table can have only **one clustered index** because the physical order of rows can only follow one order.
3. **Performance:** Speeds up queries that involve range-based operations (e.g., `BETWEEN`, `ORDER BY`).
4. **Primary Key Association:** If a primary key is defined, many DBMSs automatically create a clustered index on it.

### Non-clustered Index

A **non-clustered index** is an index structure that maintains a logical ordering of data rows based on the indexed columns, without affecting the physical storage order of the table. Instead of physically reordering the rows, a non-clustered index stores pointers (row locators) to the actual data rows.

**Key Characteristics of Non-clustered Indexes:**

1. **Logical Ordering:** The data rows are not stored in sorted order; instead, the index maintains a separate structure that points to the actual rows.
2. **Multiple Indexes Allowed:** A table can have **multiple non-clustered indexes**, each allowing efficient access based on different search criteria.
3. **Performance:** Useful for queries involving lookups that don't require a range scan or when filtering on columns that are not part of the primary key.
4. **Storage Overhead:** Non-clustered indexes require additional storage to maintain the pointer references.

### **Example: Clustered vs Non-Clustered Index**

Consider a table `EMPLOYEES` with the following schema:

```sql
CREATE TABLE EMPLOYEES (
    EmployeeID INT PRIMARY KEY,
    Name VARCHAR(50),
    Department VARCHAR(50),
    Salary DECIMAL(10,2)
);

CREATE CLUSTERED INDEX idx_emp_id ON EMPLOYEES(EmployeeID);

CREATE NONCLUSTERED INDEX idx_emp_name ON EMPLOYEES(Name);
```

| Feature            | Clustered Index             | Non-clustered Index           |
|-------------------|-----------------------------|-------------------------------|
| **Data Storage**   | Physically reorders rows     | Stores pointers to actual rows |
| **Number per Table** | Only one                     | Multiple allowed               |
| **Performance**    | Faster for range queries     | Faster for lookups              |
| **Storage Overhead** | No extra storage needed      | Requires additional storage    |
| **Usage**          | Primary key, range queries   | Secondary key, frequent lookups|

---

## Union rule in functional dependencies

**Functional dependency rule (Union Rule):**

$\text{If } \alpha \to \beta \text{ and } \alpha \to \gamma, \text{ then } \alpha \to \beta \gamma$

This means if $\alpha$ determines both $\beta$ and $\gamma$ separately, it must also determine their combination, $\alpha \to \beta \gamma$.

---

![alt text](joinscheatsheet.png)

---

## Buffer Manager in DBMS

The buffer manager is a crucial component of a database management system (DBMS) responsible for efficiently managing the buffer pool (the area of memory used for holding database pages temporarily). Here’s a breakdown of what it does:

**1. Page Replacement:**
The buffer manager’s primary task is to manage the pages that are read into memory from disk. When the buffer pool is full and a new page needs to be loaded, the buffer manager must decide which existing page to evict. This decision is made based on a replacement policy, such as Least Recently Used (LRU), Most Recently Used (MRU), or Clock. This ensures that the most relevant data is kept in memory for faster access.
**2. Disk Space Management:**
While buffer managers are responsible for memory management, they don’t manage available disk space directly. This task is typically handled by the storage manager, not the buffer manager.
**3. Clustered Indexes:**
The buffer manager does not automatically drop clustered indexes. It is responsible for loading and maintaining pages in memory, but operations on indexes (such as creation, maintenance, or dropping) are part of the database’s indexing system, not the buffer manager.
**4. User Tracking:**
The buffer manager does not typically track the number of current users for each page in the buffer pool. User management is handled by other components, such as the transaction manager, not the buffer manager.

**In Summary:**
The buffer manager in a DBMS handles the efficient loading and replacement of pages in memory (the buffer pool), ensuring that frequently accessed pages are kept in memory and that memory usage is optimized. The primary task is to decide which pages to keep and which to replace based on a replacement policy.

---

## Relational Algebra

![alt text](algebra-operators.png)

---

### Projection (π) in Relational Algebra

Projection is an operator in relational algebra that extracts specific attributes (columns) from a relation (table).

#### Notation:

$\pi_{attribute1, attribute2, \dots} (Relation)$

#### Example:

Given the relation `Student(ID, Name, Age, Department)`, the query:

$\pi_{\text{Name}, \text{Age}} (\text{Student})$

Returns:

| Name  | Age |
|-------|-----|
| Alice | 20  |
| Bob   | 22  |

#### Key Points:

- Removes unwanted attributes.
- Eliminates duplicate rows.
- Used for query optimization and simplification.

---
### Selection (σ) in Relational Algebra

The selection operator filters rows from a relation based on a specified condition.

#### Notation:

$\sigma_{\text{condition}} (\text{Relation})$

#### Example:

Given the relation `Employee(ID, Name, Age, Department, Salary)`, the query:
$\sigma_{\text{Department} = 'IT'} (\text{Employee})$

Returns:

| ID  | Name  | Age | Department | Salary |
|-----|-------|-----|------------|--------|
| 101 | Alice | 30  | IT         | 60000  |
| 103 | Bob   | 25  | IT         | 50000  |

#### Conditions:

- Comparisons: $=, \neq, >, <, \geq, \leq $
- Logical operations: AND($\land$), OR($\lor$), NOT($\neg$)
- Example: $\sigma_{\text{Age} > 30 \land \text{Department} = HR'} \text{Employee}$

#### Properties:

- **Commutative:** Order of selection does not matter.
- **Idempotent:** Applying the same selection multiple times yields the same result.

---

#### Example Combining Selection and Projection:

If you want to retrieve only the Name and Salary of employees in the IT department:


$\pi_{\text{Name}, \text{Salary}} (\sigma_{\text{Department} = IT{\prime}} (\text{Employee}))$

Returns:

| Name  | Salary |
|-------|--------|
| Alice | 60000  |
| Bob   | 50000  |

---

## Evaluation Tree

![alt text](evaluation-tree.png)

---
