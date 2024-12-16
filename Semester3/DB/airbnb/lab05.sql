

-- Create table Ta
CREATE TABLE Ta (
    aid INT PRIMARY KEY,
    a2 INT UNIQUE
);

-- Create table Tb
CREATE TABLE Tb (
    bid INT PRIMARY KEY,
    b2 INT
);

-- Create table Tc
CREATE TABLE Tc (
    cid INT PRIMARY KEY,
    aid INT,
    bid INT,
    FOREIGN KEY (aid) REFERENCES Ta(aid),
    FOREIGN KEY (bid) REFERENCES Tb(bid)
);

-- Procedure to insert rows into Ta
CREATE PROCEDURE sp_insertDummyTa
    @num INT
AS
BEGIN
    DECLARE @i INT = 1;
    DECLARE @maxAid INT = ISNULL((SELECT MAX(aid) FROM Ta), 0);

    WHILE @i <= @num
    BEGIN
        SET @maxAid = @maxAid + 1;
        INSERT INTO Ta (aid, a2) VALUES (@maxAid, @maxAid * 10);
        SET @i = @i + 1;
    END
END;
GO

-- Procedure to insert rows into Tb
CREATE PROCEDURE sp_insertDummyTb
    @num INT
AS
BEGIN
    DECLARE @i INT = 1;
    DECLARE @maxBid INT = ISNULL((SELECT MAX(bid) FROM Tb), 0);

    WHILE @i <= @num
    BEGIN
        SET @maxBid = @maxBid + 1;
        INSERT INTO Tb (bid, b2) VALUES (@maxBid, @maxBid * 100);
        SET @i = @i + 1;
    END
END;
GO

-- Procedure to insert rows into Tc
CREATE PROCEDURE sp_insertDummyTc
    @num INT
AS
BEGIN
    DECLARE @i INT = 1;
    DECLARE @maxCid INT = ISNULL((SELECT MAX(cid) FROM Tc), 0);
    DECLARE @maxAid INT = ISNULL((SELECT MAX(aid) FROM Ta), 0);
    DECLARE @maxBid INT = ISNULL((SELECT MAX(bid) FROM Tb), 0);

    WHILE @i <= @num
    BEGIN
        SET @maxCid = @maxCid + 1;
        INSERT INTO Tc (cid, aid, bid) VALUES (@maxCid, @maxAid, @maxBid);
        SET @i = @i + 1;
    END
END;
GO

-- Insert data into Ta
INSERT INTO Ta (aid, a2) VALUES
(1, 10), (2, 20), (3, 30), (4, 40), (5, 50);

-- Insert data into Tb
INSERT INTO Tb (bid, b2) VALUES
(1, 100), (2, 200), (3, 300), (4, 400), (5, 500);

-- Insert data into Tc
INSERT INTO Tc (cid, aid, bid) VALUES
(1, 1, 1), (2, 2, 2), (3, 3, 3), (4, 4, 4), (5, 5, 5);

-- a) Queries on Ta
-- 1) Clustered index scan
SELECT * FROM Ta;

-- 2) Clustered index seek
SELECT * FROM Ta WHERE aid = 1;

-- 3) Nonclustered index scan
CREATE NONCLUSTERED INDEX idx_a2 ON Ta(a2);
DROP INDEX idx_a2 ON Ta;
SELECT * FROM Ta WHERE a2 > 10;

 -- 4) Nonclustered index seek
SELECT * FROM Ta WHERE a2 = 10;

-- 5) Key lookup
SELECT aid FROM Ta WHERE a2 = 10;


-- b) Queries on Tb (without index it works slower, should work better on large data sets)
-- 1) Query and execution plan without index
SELECT * FROM Tb WHERE b2 = 200; -- 92 ms; total cost: 0.0032875
-- 2) Create nonclustered index on Tb
CREATE NONCLUSTERED INDEX idx_b2 ON Tb(b2);
DROP INDEX idx_b2 ON Tb;
-- 3) Query and execution plan with index
SELECT * FROM Tb WHERE b2 = 200; -- ~40 ms

-- c) Create a view and analyze indexes

-- 1) Create a view
CREATE VIEW vw_Ta_Tb AS
SELECT Ta.aid, Ta.a2, Tb.bid, Tb.b2
FROM Ta
JOIN Tc ON Ta.aid = Tc.aid
JOIN Tb ON Tb.bid = Tc.bid;

-- 2) Check existing indexes
-- EXEC sp_helpindex 'Ta';
-- EXEC sp_helpindex 'Tb';
-- EXEC sp_helpindex 'Tc';

SELECT * FROM vw_Ta_Tb;

-- with data cost 0.05

DELETE FROM Tc;
DELETE FROM Tb;
DELETE FROM Ta;
EXEC sp_insertDummyTa 1000;
EXEC sp_insertDummyTb 1000;
EXEC sp_insertDummyTc 1000;

-- 005863
-- 05298
-- 0051227
-- 0058634