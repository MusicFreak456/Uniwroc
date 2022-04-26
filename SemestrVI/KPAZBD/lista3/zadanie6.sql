drop procedure if exists discontinue;
drop type if exists disc_table_type;

create type disc_table_type as 
  table (id int primary key, DiscontinuedDate date not null)
go

create procedure discontinue
  @table disc_table_type readonly
  as
    declare @notnull_count int = 
      (select count(Product.DiscontinuedDate) 
        from @table as t
        join SalesLT.Product
        on Product.ProductID = t.id);
    if @notnull_count > 0 
      throw 50000, 'Na jednym z produktów jest już ustawiona data', 0;

    update SalesLT.Product
    set Product.DiscontinuedDate = t.DiscontinuedDate
    from SalesLT.Product
    join @table as t
      on Product.ProductID = t.id
go

declare @table disc_table_type;
insert into @table values (680, getdate()), (706, dateadd(day,2,getdate()))
exec discontinue @table
