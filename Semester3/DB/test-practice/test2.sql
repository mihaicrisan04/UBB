use db_test2;

/* Part I

1.
SELECT C2, SUM(C3)
FROM Songs
WHERE C3>28 OR C4 < 25
GROUP BY C2
HAVING C2 LIKE 'P%'

U2 38
The rolling stones 11
Led Zeppelin 45
-- Pink Floyd 98
Qeen 62
Michael Jackson 35

Result: 1
Answer: e.

2.
1 result:
Michael Jackson

Answer: b

3.
    d   i
7 1 21 21
2 2 54 21
7 2 84 21
5 5 65 21

result: 0
Answer: d.
*/



-- Part II

-- 1. data model

CREATE TABLE TaxCompanies (
    TaxCompID INT PRIMARY KEY,
    Name VARCHAR(100) NOT NULL,
    NoOfClients INT NOT NULL,
    NoOfSRLs INT NOT NULL
);

CREATE TABLE Clients (
    ClientID INT PRIMARY KEY,
    TaxCompID INT NOT NULL,
    IdentificationNumber VARCHAR(50) NOT NULL,
    MoneyAtDisposal DECIMAL(10, 2) NOT NULL,
    FOREIGN KEY (TaxCompID) REFERENCES TaxCompanies(TaxCompID)
);

CREATE TABLE SRLs (
    SRLID INT PRIMARY KEY,
    ClientID INT NOT NULL,
    Name VARCHAR(100) NOT NULL,
    Activity VARCHAR(100) NOT NULL,
    Location VARCHAR(100) NOT NULL,
    FOREIGN KEY (ClientID) REFERENCES Clients(ClientID)
);

CREATE TABLE Assets (
    AssetID INT PRIMARY KEY,
    ClientID INT NOT NULL,
    Name VARCHAR(100) NOT NULL,
    NumberOfAssets INT NOT NULL,
    Location VARCHAR(100) NOT NULL,
    FOREIGN KEY (ClientID) REFERENCES Clients(ClientID)
);


-- mock data
insert into TaxCompanies (TaxCompID, Name, NoOfClients, NoOfSRLs)
values
    (1, 'TaxComp1', 10, 5),
    (2, 'TaxComp2', 20, 10);

insert into Clients (ClientID, TaxCompID, IdentificationNumber, MoneyAtDisposal)
values
    (1, 1, '123456', 1000.00),
    (2, 1, '654321', 2000.00),
    (3, 2, '111111', 3000.00),
    (4, 2, '222222', 4000.00);

insert into SRLs (SRLID, ClientID, Name, Activity, Location)
values
    (1, 1, 'SRL1', 'Activity1', 'Location1'),
    (2, 1, 'SRL2', 'Activity2', 'Location2'),
    (3, 2, 'SRL3', 'Activity3', 'Location3'),
    (4, 2, 'SRL4', 'Activity4', 'Location4');

insert into Assets (AssetID, ClientID, Name, NumberOfAssets, Location)
values
    (1, 1, 'Asset1', 10, 'Location1'),
    (2, 1, 'Asset2', 20, 'Location2'),
    (3, 2, 'Asset3', 30, 'Location3'),
    (4, 2, 'Asset4', 40, 'Location4');



-- 2. procedure
create or alter procedure sp_GetClientAssetsAndSRLs
    @ClientID int
as
begin
    select
        c.ClientID,
        sum(a.NumberOfAssets) as NumberOfAssets,
        count(srl.Name) as NumberOfSRLs
    from
        Clients c
    left join
        Assets A on c.ClientID = A.ClientID
    left join SRLs srl on c.ClientID = srl.ClientID
    where c.ClientID = @ClientID
    group by c.ClientID
end

CREATE PROCEDURE GetClientAssetsAndSRLs2
    @ClientID INT
AS
BEGIN
    SELECT
        (SELECT COUNT(*) FROM Assets WHERE ClientID = @ClientID) AS NumberOfAssets,
        (SELECT COUNT(*) FROM SRLs WHERE ClientID = @ClientID) AS NumberOfSRLs;
END;

exec sp_GetClientAssetsAndSRLs 1;
exec GetClientAssetsAndSRLs2 1;


-- 3.view

create view ClientSRLsInfo as
select
    c.IdentificationNumber,
    c.MoneyAtDisposal,
    srl.Name,
    srl.Activity
from
    Clients c
join
    SRLs srl on c.ClientID = srl.ClientID

select * from ClientSRLsInfo;



-- 4. function
CREATE FUNCTION ClientSRLAssets()
RETURNS TABLE
AS
RETURN
SELECT
    c.ClientID,
    s.Location AS SRLLocation,
    COUNT(a.AssetID) AS NumberOfAssets
FROM
    Clients c
JOIN
    SRLs s ON c.ClientID = s.ClientID
LEFT JOIN
    Assets a ON c.ClientID = a.ClientID AND s.Location = a.Location
GROUP BY
    c.ClientID, s.Location;

SELECT * FROM ClientSRLAssets();







