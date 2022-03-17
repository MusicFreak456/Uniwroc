
-- select * from SalesLT.SalesOrderHeader

declare @now datetime = getdate()
declare @cnt int = 0;

while @cnt < 5
begin
   insert into SalesLT.SalesOrderHeader(CustomerID, OrderDate, DueDate, ShipMethod) values
        (1, dateadd(day, -2, @now), dateadd(day, -1 , @now), '-');
   set @cnt = @cnt + 1;
end;

set @cnt = 0;

while @cnt < 5
begin
   insert into SalesLT.SalesOrderHeader(CustomerID, OrderDate, DueDate, ShipMethod) values
        (1, dateadd(day, -2, @now), dateadd(day, 10 , @now), '-');
   set @cnt = @cnt + 1;
end;

-- delete from SalesLT.SalesOrderHeader where Status != 5;
