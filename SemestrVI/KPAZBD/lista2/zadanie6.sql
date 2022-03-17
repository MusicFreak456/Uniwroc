drop table if exists OrdersToProcess;

create table OrdersToProcess (
    SalesOrderID INT,
    Delayed BIT
);

merge OrdersToProcess as target
using SalesLT.SalesOrderHeader as source
on target.SalesOrderID = source.SalesOrderID
when matched and source.Status < 5 then
    update set target.Delayed = 
        iif(source.DueDate < getdate(), 1, 0)
when matched and source.Status >= 5 then delete
when not matched by target and source.Status < 5 then
    insert (SalesOrderID, Delayed) values (
        source.SalesOrderID,
        iif(source.DueDate < getdate(), 1, 0)
    )
when not matched by source then delete;

-- select * from OrdersToProcess;
-- select * from SalesLT.SalesOrderHeader;