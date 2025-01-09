use db_test1;

CREATE TABLE VideoGames (
    FK1 INT NOT NULL,
    FK2 INT NOT NULL,
    C1 VARCHAR(50), -- Game title
    C2 VARCHAR(50), -- Company name
    C3 INT,         -- Some numerical value (e.g., score, metric)
    C4 INT,         -- Another numerical value (e.g., rating, metric)
    C5 VARCHAR(10), -- Some additional data (e.g., category, tag)
    PRIMARY KEY (FK1, FK2) -- Composite primary key
);

-- Insert data into the table
INSERT INTO VideoGames (FK1, FK2, C1, C2, C3, C4, C5)
VALUES
(1, 1, 'God of War', 'Sony', 11, 10, 'LO'),
(2, 2, 'Minecraft', 'Microsoft', 13, 0, 'GH'),
(3, 3, 'PUBG', 'Tencent', 30, 110, 'GH'),
(4, 3, 'Pokemon', 'Nintendo', 250, 55, 'LO'),
(5, 4, 'Call of Duty', 'Blizzard', 30, 10, 'GT'),
(6, 5, 'Fifa 2023', 'Electronic Arts', 150, 310, 'GH'),
(7, 6, 'Fortnite', 'Epic Games', 45, 150, 'KU'),
(8, 7, 'Elden Ring', 'Bandai', 78, 89, 'KU'),
(9, 8, 'NBA', 'Interactive Software', 21, 81, 'OI');



-- I. 1
SELECT C2, SUM(C3) TotalC3, AVG(C4) AvgC4
FROM VideoGames
WHERE C3 >= 30 AND C4 >= 20
GROUP BY C2
HAVING AVG(C4) < 80 AND SUM(C3) < 20;

-- I. 2
SELECT
    A.C2
FROM
    VideoGames A
INNER JOIN
    VideoGames B
ON
    A.FK1 = B.FK1 AND A.FK2 = B.FK2
WHERE
    A.C3 > 100 OR B.C4 > 100 AND A.C2 LIKE 'E%';


-- Part I

/*
1.
Answer: e.

2.
Epic Games
Electronic Arts
Nintendo

Answer: e.

3.
     d   i
4 1  250 30 x
4 3  30  30 x
5 4  78  30 x
5 5  21  30 ok

51
Answer: a
 */


-- Part II

-- 1. data model
create table Clients (
    ClientID int primary key,
    Name varchar(100) not null
)

create table Accounts (
    AccountID int primary key,
    ClientID int not null unique,
    InvestingMoney int not null,
    foreign key (ClientID) references Clients(ClientID)
)

create table Cryptos (
    CryptoID int primary key,
    CryptoName varchar(50) not null unique
)

create table Transactions (
    TransactionID int primary key,
    AccountID int not null,
    CryptoID int not null,
    TransactionType varchar(10) not null check(TransactionType in ('buy', 'sell')),
    RegisteredDate date not null,
    Price int not null,
    Amount int not null,
    foreign key (AccountID) references Accounts(AccountID),
    foreign key (CryptoID) references Cryptos(CryptoID)
)

create table [Statistics] (
    StatisticsID int primary key,
    AccountID int not null,
    NumberOfBuyOrders int not null,
    NumberOfSellOrders int not null,
    TotalNumberOfOperations int not null,
    AmountLeftForInvesting  int not null,
    foreign key (AccountID) references Accounts(AccountID)
)

-- add some data
insert into Clients (ClientID, Name)
values
    (1, 'Alice'),
    (2, 'Bob'),
    (3, 'Charlie'),
    (4, 'David'),
    (5, 'Eve');

insert into Accounts (AccountID, ClientID, InvestingMoney)
values
    (1, 1, 1000),
    (2, 2, 2000),
    (3, 3, 3000),
    (4, 4, 4000),
    (5, 5, 5000);

insert into Cryptos (CryptoID, CryptoName)
values
    (1, 'Bitcoin'),
    (2, 'Ethereum'),
    (3, 'Litecoin'),
    (4, 'Ripple'),
    (5, 'Dogecoin');

insert into Transactions (TransactionID, AccountID, CryptoID, TransactionType, RegisteredDate, Price, Amount)
values
    (1, 1, 1, 'buy', '2022-01-01', 50000, 1),
    (2, 1, 2, 'buy', '2022-01-02', 3000, 2),
    (3, 1, 3, 'buy', '2022-01-03', 1000, 3),
    (4, 1, 4, 'buy', '2022-01-04', 500, 4),
    (5, 1, 5, 'buy', '2022-01-05', 100, 5),
    (6, 2, 1, 'buy', '2022-01-06', 50000, 1),
    (7, 2, 2, 'buy', '2022-01-07', 3000, 2),
    (8, 2, 3, 'buy', '2022-01-08', 1000, 3),
    (9, 2, 4, 'buy', '2022-01-09', 500, 4),
    (10, 2, 5, 'buy', '2022-01-10', 100, 5),
    (11, 3, 1, 'buy', '2022-01-11', 50000, 1),
    (12, 3, 2, 'buy', '2022-01-12', 3000, 2),
    (13, 3, 3, 'buy', '2022-01-13', 1000, 3),
    (14, 3, 4, 'buy', '2022-01-14', 500, 4),
    (15, 3, 5, 'buy', '2022-01-15', 100, 5),
    (16, 4, 1, 'buy', '2022-01-16', 50000, 1),
    (17, 4, 2, 'buy', '2022-01-17', 3000, 2),
    (18, 4, 3, 'buy', '2022-01-18', 1000, 3);

-- 5 sell transactions
insert into Transactions (TransactionID, AccountID, CryptoID, TransactionType, RegisteredDate, Price, Amount)
values
    (19, 1, 1, 'sell', '2022-01-19', 50000, 1),
    (20, 1, 2, 'sell', '2022-01-20', 3000, 2),
    (21, 1, 3, 'sell', '2022-01-21', 1000, 3),
    (22, 2, 1, 'sell', '2022-01-22', 50000, 1),
    (23, 2, 2, 'sell', '2022-01-23', 3000, 2);

insert into [Statistics] (StatisticsID, AccountID, NumberOfBuyOrders, NumberOfSellOrders, TotalNumberOfOperations, AmountLeftForInvesting)
values
    (1,1, 5, 3, 8, 0),
    (2, 2, 5, 2, 7, 0),
    (3, 3, 5, 0, 5, 0),
    (4, 4, 3, 0, 3, 0),
    (5, 5, 0, 0, 0, 0);


-- 2. procedure
create procedure GetClientCryptoInfo
    @clientId int
as
begin
    select
        c.CryptoName,
        a.InvestingMoney - sum(t.amount * t.price) as RemainingMoney
    from
        Clients cl
    join
        Accounts a on cl.ClientID = a.ClientID
    left join
        Transactions t on a.AccountID = t.AccountID
    left join
        Cryptos c on t.CryptoID = c.CryptoID
    where
        cl.ClientID = @clientId
    group by
        c.CryptoName, a.InvestingMoney;
end
go

exec GetClientCryptoInfo 4;



-- 3. view
CREATE VIEW ClientTransactionSummary AS
SELECT
    cl.ClientID,
    cl.Name,
    SUM(CASE WHEN t.TransactionType = 'buy' THEN 1 ELSE 0 END) AS BuyTransactions,
    SUM(CASE WHEN t.TransactionType = 'sell' THEN 1 ELSE 0 END) AS SellTransactions
FROM
    Clients cl
LEFT JOIN
    Accounts a ON cl.ClientID = a.ClientID
LEFT JOIN
    Transactions t ON a.AccountID = t.AccountID
GROUP BY
    cl.ClientID, cl.Name;

SELECT * FROM ClientTransactionSummary;



-- 4. function
CREATE FUNCTION ClientsInvestingOnlyInBitcoin()
RETURNS TABLE
AS
RETURN
SELECT
    cl.ClientID,
    COUNT(t.TransactionID) AS NumberOfTransactions
FROM
    Clients cl
JOIN
    Accounts a ON cl.ClientID = a.ClientID
JOIN
    Transactions t ON a.AccountID = t.AccountID
JOIN
    Cryptos c ON t.CryptoID = c.CryptoID
WHERE
    c.CryptoName = 'Bitcoin'
GROUP BY
    cl.ClientID

SELECT * FROM ClientsInvestingOnlyInBitcoin();
