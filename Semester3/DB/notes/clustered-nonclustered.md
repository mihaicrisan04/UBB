## Clustered vs. Non-Clustered Indexes

Indexes in databases improve query performance by allowing faster retrieval of records. There are two primary types of indexes: **Clustered Indexes** and **Non-Clustered Indexes**.

---

### 1. Clustered Index
**Definition:**  
A clustered index determines the physical order of data in a table based on the indexed column(s). A table can have only **one clustered index** because data rows can be sorted in only one order.

**Key Points:**
- The data is physically sorted according to the indexed column.  
- Faster for range queries and sorting operations.  
- Slower for insert and update operations if the order needs to be maintained.  
- Typically created on primary keys by default.  

**Example:**  
Consider a table `Students`:

| StudentID | Name  | Age |
|-----------|-------|-----|
| 101       | Alice | 22  |
| 102       | Bob   | 21  |
| 103       | Carol | 23  |

If a **clustered index** is created on `StudentID`, the rows will be physically stored in ascending order of `StudentID`.  
```sql
CREATE CLUSTERED INDEX idx_StudentID ON Students(StudentID);
```

**When to use:**  
- When searching for a range of values (`BETWEEN`, `<`, `>`, etc.).  
- When frequent sorting is required.  

---

### 2. Non-Clustered Index
**Definition:**  
A non-clustered index stores a pointer to the actual data rather than sorting it physically. It creates a separate structure that holds key values and their respective row locations in the table.

**Key Points:**
- The data is not physically sorted; only pointers to data are maintained.  
- A table can have **multiple non-clustered indexes**.  
- Useful for searching specific columns without affecting data storage order.  
- Involves extra storage as the index exists separately.  

**Example:**  
If we create a **non-clustered index** on the `Name` column in the `Students` table, it allows faster searches based on names.

```sql
CREATE NONCLUSTERED INDEX idx_Name ON Students(Name);
```

**When to use:**  
- When searching for exact matches (e.g., `WHERE Name = 'Alice'`).  
- When dealing with columns frequently filtered in `WHERE` clauses but not used for sorting.

---

### Key Differences Between Clustered and Non-Clustered Indexes

| Feature            | Clustered Index                         | Non-Clustered Index                     |
|-------------------|---------------------------------------|-----------------------------------------|
| Data Storage      | Physically reorders data              | Stores a pointer to actual data         |
| Number per Table  | Only one                              | Multiple allowed                        |
| Speed             | Faster for range queries              | Slower for range queries                |
| Storage Space     | No extra storage needed               | Requires additional storage             |
| Default Use Case  | Primary key                           | Frequently searched columns             |

---

### Summary:
- **Use clustered indexes** for primary key fields or frequently sorted columns.  
- **Use non-clustered indexes** for lookup fields and frequently searched columns.  

