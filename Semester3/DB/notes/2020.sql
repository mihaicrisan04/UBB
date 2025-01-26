


-- II. 2.a)
-- For every orchestra that ordered violins in at least 3 different orders, find the total number of ordered instruments – in SQL,
-- without views (CustomerID, TotalNumInstruments).

select 
    c.CustomerID,
    sum(od.Quantity) as TotalNumberOfInstruments
from Customers c
join Orders o on c.CustomerID = o.CustomerID
join OrderDetails od on o.OrderID = od.OrderID
where c.Type = 'orchestra'
group by c.CustomerID
having (
        select count(distinct i2.Color)
        from Orders o2
        join OrderDetails od2 on o2.OrderID = od2.OrderID
        join Instruments i2 on od2.InstruementID = i2.p2InstruementID
        join Subcategories s2 on i2.SubcategoryID = s2.SubcategoryID
        where o2.CustomerID = c.CustomerID and s2.name = 'violin'
    ) > 3;