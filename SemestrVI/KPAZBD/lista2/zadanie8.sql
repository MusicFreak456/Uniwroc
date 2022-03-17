alter table SalesLT.SalesOrderHeader with nocheck 
    add constraint CK_SalesOrderHeader_ShipDate 
        check (ShipDate>=OrderDate or ShipDate is null);

declare @now datetime = getdate();

insert into SalesLT.SalesOrderHeader(CustomerID, OrderDate, ShipDate , DueDate, ShipMethod) values
        (1, dateadd(day, -2, @now), dateadd(day, -3, @now), dateadd(day, -1 , @now), '-');

alter table SalesLT.SalesOrderHeader 
    nocheck constraint CK_SalesOrderHeader_ShipDate;
    
-- ponowna próba

alter table SalesLT.SalesOrderHeader 
    check constraint CK_SalesOrderHeader_ShipDate;

dbcc checkconstraints('SalesLT.SalesOrderHeader');