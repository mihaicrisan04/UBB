## What is Normalization?
Normalization is a process in database design used to organize data to reduce redundancy and improve data integrity. It involves decomposing tables to eliminate duplicate data and ensure dependencies are logically stored.

---

## Step-by-Step Normal Forms

### 1. First Normal Form (1NF)
**Description:**
- Ensures that all attributes contain atomic (indivisible) values, and each column contains only one value per row.

**Requirements:**
- Remove repeating groups (multivalued attributes).
- Ensure each column holds a single value of a specific type.

**Example (Not in 1NF):**
```plaintext
Student | Courses
--------|-----------------
John    | Math, Physics
```
**After 1NF:**
```plaintext
Student | Course
--------|--------
John    | Math
John    | Physics
```

---

### 2. Second Normal Form (2NF)
**Description:**
- Ensures that all non-prime attributes (not part of any candidate key) are fully functionally dependent on the whole primary key.

**Requirements:**
- Must be in 1NF.
- Remove partial dependencies (where a non-key attribute depends only on part of a composite key).

**Example (Not in 2NF):**
```plaintext
StudentID | Course | Instructor
----------|--------|-----------
1         | Math   | Prof. A
1         | Physics| Prof. B
```
**After 2NF:**
```plaintext
STUDENT_COURSE(StudentID, Course)
COURSE_INSTRUCTOR(Course, Instructor)
```

---

### 3. Third Normal Form (3NF)
**Description:**
- Ensures that no transitive dependency exists, meaning non-prime attributes should depend only on the primary key, not indirectly through another non-prime attribute.

**Requirements:**
- Must be in 2NF.
- Remove transitive dependencies (where a non-key attribute depends on another non-key attribute).

**Example (Not in 3NF):**
```plaintext
StudentID | Course  | Instructor | Department
----------|---------|------------|-----------
1         | Math    | Prof. A     | Science
```
**After 3NF:**
```plaintext
STUDENT_COURSE(StudentID, Course, Instructor)
INSTRUCTOR_DEPT(Instructor, Department)
```

---

### 4. Boyce-Codd Normal Form (BCNF)
**Description:**
- A stricter version of 3NF where every determinant (attribute that determines others) must be a candidate key.

**Requirements:**
- Must be in 3NF.
- For any dependency X → Y, X should be a superkey.

**Example (Not in BCNF):**
```plaintext
Course  | Instructor | Room
--------|------------|------
Math    | Prof. A     | R101
Physics | Prof. A     | R101
```
**After BCNF:**
```plaintext
COURSE_INSTRUCTOR(Course, Instructor)
INSTRUCTOR_ROOM(Instructor, Room)
```

---

### 5. Fourth Normal Form (4NF)
**Description:**
- Ensures no multi-valued dependencies exist, meaning an attribute should depend only on the primary key, not on multiple independent values.

**Requirements:**
- Must be in BCNF.
- Remove multi-valued dependencies.

**Example (Not in 4NF):**
```plaintext
Employee | Skill   | Project
---------|---------|---------
John     | Python  | AI
John     | Java    | AI
John     | Python  | Web
```
**After 4NF:**
```plaintext
EMPLOYEE_SKILL(Employee, Skill)
EMPLOYEE_PROJECT(Employee, Project)
```

---

### 6. Fifth Normal Form (5NF or PJNF)
**Description:**
- Ensures that a relation is broken down into smaller relations without losing information by eliminating join dependencies.

**Requirements:**
- Must be in 4NF.
- Decompose to eliminate join dependencies.

**Example:**
- If splitting a table into multiple tables causes loss of data integrity, it’s not in 5NF.

---

### 7. Sixth Normal Form (6NF)
**Description:**
- Deals with temporal databases where records change over time, ensuring data redundancy is minimized while maintaining history.

**Requirements:**
- Must be in 5NF.
- Applied in very specific use cases like time-series data.

---

## Summary Table of Normal Forms

| Normal Form | Key Concept                          | Goal                                 |
|-------------|-------------------------------------|--------------------------------------|
| 1NF         | Eliminate repeating groups         | Atomicity                           |
| 2NF         | Remove partial dependencies        | Full functional dependency          |
| 3NF         | Remove transitive dependencies     | No dependency on non-prime attributes |
| BCNF        | Ensure every determinant is a key  | Stronger than 3NF                    |
| 4NF         | Remove multi-valued dependencies   | Independent attributes               |
| 5NF         | Remove join dependencies           | Lossless decomposition               |
| 6NF         | Handle temporal data               | Preserve historical records          |
