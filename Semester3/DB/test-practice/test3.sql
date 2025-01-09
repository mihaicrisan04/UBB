use db_test3;


-- Part II

create table Trains (
    TrainID int primary key,
    TrainTypeID int not null,
    Name varchar(50) not null,
    foreign key (TrainTypeID) references TrainTypes(TrainTypeID)
)

create table TrainTypes (
    TrainTypeID int primary key,
    Description varchar(100) not null
)

create table Stations (
    StationID int primary key,
    Name varchar(50) not null unique
)

create table Routes (
    RouteID int primary key,
    TrainID int not null,
    Name varchar(50) not null unique,
    foreign key (TrainID) references Trains(TrainID)
)

create table Stops (
    StopID int primary key,
    StationID int not null,
    RouteID int not null,
    ArrivalTime time not null,
    DepartureTime time not null,
    foreign key (StationID) references Stations(StationID),
    foreign key (RouteID) references Routes(RouteID),
    unique (StationID, RouteID)
)


CREATE PROCEDURE AddOrUpdateStationOnRoute
    @RouteID INT,
    @StationID INT,
    @ArrivalTime TIME,
    @DepartureTime TIME
AS
BEGIN
    IF EXISTS (SELECT 1 FROM RouteStations WHERE RouteID = @RouteID AND StationID = @StationID)
    BEGIN
        UPDATE RouteStations
        SET ArrivalTime = @ArrivalTime, DepartureTime = @DepartureTime
        WHERE RouteID = @RouteID AND StationID = @StationID;
    END
    ELSE
    BEGIN
        INSERT INTO RouteStations (RouteID, StationID, ArrivalTime, DepartureTime)
        VALUES (@RouteID, @StationID, @ArrivalTime, @DepartureTime);
    END
END;

CREATE VIEW RoutesThroughAllStations AS
SELECT
    r.Name AS RouteName
FROM
    Routes r
JOIN
    RouteStations rs ON r.RouteID = rs.RouteID
GROUP BY
    r.Name
HAVING
    COUNT(DISTINCT rs.StationID) = (SELECT COUNT(*) FROM Stations);


CREATE FUNCTION StationsWithMoreThanRRoutes(@R INT)
RETURNS TABLE
AS
RETURN
SELECT
    s.Name AS StationName,
    COUNT(rs.RouteID) AS NumberOfRoutes
FROM
    Stations s
JOIN
    RouteStations rs ON s.StationID = rs.StationID
GROUP BY
    s.Name
HAVING
    COUNT(rs.RouteID) > @R;