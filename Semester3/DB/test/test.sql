use practical_exam;


-- data model
create table Metros (
    MetroID int primary key,
    MetroTypeID int not null,
    YearOfProduction int not null,
    MaxSpeed int not null,
    foreign key (MetroTypeID) references MetroTypes(MetroTypeID)
)

create table MetroTypes (
    MetroTypeID int primary key,
    Description varchar(100) not null
)

create table Drivers (
    DriverID int primary key,
    Email varchar(100) not null,
    YearOfBirth int not null,
    PhoneNumber varchar(20) not null
)

create table Stations (
    StationID int primary key,
    Name varchar(50) not null,
    ArrivalTime time,
    DepartureTime time
)

create table Routes (
    RouteID int primary key,
    Name varchar(20) not null,
    MetroID int not null,
    foreign key (MetroID) references Metros(MetroID)
)

create table RouteStations (
    RouteStationID int primary key ,
    RouteID int not null,
    StationID int not null,
    ArrivalTime time not null,
    DepartureTime time not null,
    foreign key (RouteID) references Routes(RouteID),
    foreign key (StationID) references Stations(StationID),
    unique (RouteID, StationID)
)

-- mock data
insert into MetroTypes(MetroTypeID, Description)
values
    (1, 'Type 1'),
    (2, 'Type 2'),
    (3, 'Type 3'),
    (4, 'Type 4'),
    (5, 'Type 5');

insert into Drivers(DriverID, Email, YearOfBirth, PhoneNumber)
values
    (1, 'mail1', 1990, '1234567890'),
    (2, 'mail2', 1991, '1234567891'),
    (3, 'mail3', 1992, '1234567892'),
    (4, 'mail4', 1993, '1234567893'),
    (5, 'mail5', 1994, '1234567894');

insert into Stations(StationID, Name, ArrivalTime, DepartureTime)
values
    (1, 'Station 1', '10:00', '10:10'),
    (2, 'Station 2', '10:20', '10:30'),
    (3, 'Station 3', '10:40', '10:50'),
    (4, 'Station 4', '11:00', '11:10'),
    (5, 'Station 5', '11:20', '11:30');

insert into Metros(MetroID, MetroTypeID, YearOfProduction, MaxSpeed)
values
    (1, 1, 2010, 100),
    (2, 2, 2011, 110),
    (3, 3, 2012, 120),
    (4, 4, 2013, 130),
    (5, 5, 2014, 140);

insert into Routes(RouteID, Name, MetroID)
values
    (1, 'Route 1', 1),
    (2, 'Route 2', 2),
    (3, 'Route 3', 3),
    (4, 'Route 4', 4),
    (5, 'Route 5', 5);

insert into RouteStations(RouteStationID, RouteID, StationID, ArrivalTime, DepartureTime)
values
    (1, 1, 1, '10:00', '10:10'),
    (2, 1, 2, '10:20', '10:30'),
    (3, 1, 3, '10:40', '10:50'),
    (4, 1, 4, '11:00', '11:10'),
    (5, 1, 5, '11:20', '11:30');

insert into RouteStations(RouteStationID, RouteID, StationID, ArrivalTime, DepartureTime)
values
    (6, 2, 1, '12:00', '12:10'),
    (7, 2, 2, '12:20', '12:30'),
    (8, 2, 3, '12:40', '12:50');

insert into RouteStations(RouteStationID, RouteID, StationID, ArrivalTime, DepartureTime)
values
    (9, 3, 1, '13:00', '13:10'),
    (10, 3, 2, '13:20', '13:30'),
    (11, 3, 3, '13:40', '13:50'),
    (12, 3, 4, '14:00', '14:10');



-- procedure
drop procedure AddStationToRoute;
create procedure AddStationToRoute
    @RouteID int,
    @StationID int,
    @ArrivalTime time,
    @DepartureTime time
as
begin
    if EXISTS (select 1 from RouteStations where RouteID = @RouteID and StationID = @StationID)
    begin
        update RouteStations
        set ArrivalTime = @ArrivalTime, DepartureTime = @DepartureTime
        where RouteID = @RouteID and StationID = @StationID;
    end
    else
    begin
        insert into RouteStations (RouteStationID, RouteID, StationID, ArrivalTime, DepartureTime)
        values (100, @RouteID, @StationID, @ArrivalTime, @DepartureTime);
    end
end;
go

-- new station on route
exec AddStationToRoute 2, 4, '13:00', '13:10';
-- update stations ar time and dp time
exec AddStationToRoute 1, 1, '10:00', '10:10';

-- view
create view TopTwoMostVisitedStations as
select top 2
    s.Name as StationName,
    COUNT(rs.RouteID) as VisitCount
from
    Stations s
join
    RouteStations rs on s.StationID = rs.StationID
group by
    s.Name
order by
    VisitCount desc

select * from TopTwoMostVisitedStations;



-- function
create function StationsOnMoreThanNRoutes(@N int)
returns table
as
return
select
    s.Name as StationName,
    COUNT(rs.RouteID) as NumberOfRoutes
from
    Stations s
join
    RouteStations rs on s.StationID = rs.StationID
group by
    s.Name
having
    COUNT(rs.RouteID) > @N;

select * from StationsOnMoreThanNRoutes(1);



