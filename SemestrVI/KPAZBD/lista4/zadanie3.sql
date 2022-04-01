drop table if exists [Buffer], History, Parameters;
drop trigger if exists BufferInsertTrigger;
drop type if exists HistoryTable;

create type HistoryTable as
  table (
    ID int,
    AddressUrl varchar(20),
    LastEntry datetime
  );


create table [Buffer] (
  ID int identity(1,1) primary key,
  AddressUrl varchar(20),
  LastEntry datetime
);

create table History (
  ID int identity(1,1) primary key,
  AddressUrl varchar(20),
  LastEntry datetime
);

create table Parameters (
  [Name] varchar(10) primary key,
  [Value] int
);

go

insert into Parameters values
  ('max_cache', 2);
go

create trigger HistoryInsertTrigger on [History]
instead of insert as begin 
  update History
  set LastEntry = i.LastEntry
    from History join inserted i
      on i.AddressUrl = History.AddressUrl;

  insert into History select i.AddressUrl, i.LastEntry
    from inserted i
    left join History h
    on h.AddressUrl = i.AddressUrl
    where h.AddressUrl is null;
end

go

create trigger BufferInsertTrigger on [Buffer] 
instead of insert as begin
  update [Buffer]
  set LastEntry = i.LastEntry
    from [Buffer] join inserted i
      on i.AddressUrl = [Buffer].AddressUrl;

  declare @to_be_inserted HistoryTable;
  insert into @to_be_inserted select i.ID, i.AddressUrl, i.LastEntry
    from inserted i
    left join [Buffer] b
    on b.AddressUrl = i.AddressUrl
    where b.AddressUrl is null;
  
  declare @cache_limit int, @free_rows int, @insert_num int;
  set @insert_num=@@rowcount;

  select @cache_limit=[Value] from Parameters where [Name]='max_cache';
  select @free_rows=@cache_limit - count(ID) from [Buffer];
  
  declare @to_be_deleted HistoryTable;
  insert into @to_be_deleted 
    select top( greatest(@insert_num - @free_rows, 0) ) * 
    from [Buffer] order by LastEntry asc;

  delete from [Buffer] 
    from [Buffer] 
    join @to_be_deleted t 
      on [Buffer].ID = t.ID;

  insert into History select AddressUrl, LastEntry from @to_be_deleted;
  insert into [Buffer] select 
    top(least(@insert_num, @cache_limit)) AddressUrl, LastEntry
    from @to_be_inserted
    order by AddressUrl asc;

  insert into [History] select 
    top(greatest(@insert_num - @cache_limit,0)) AddressUrl, LastEntry
    from @to_be_inserted
    order by AddressUrl desc;
end

go

-- insert into [Buffer] values
--   ('www.example.com', getdate()),
--   ('www.example2.com', getdate())

-- insert into [Buffer] values ('www.example.com', getdate());
-- insert into [Buffer] values ('www.example1.com', getdate());
-- insert into [Buffer] values ('www.example4.com', getdate());
-- insert into [Buffer] values 
--   ('www.example3.com', getdate()),
--   ('www.example4.com', getdate());
-- insert into [Buffer] values 
--   ('www.example5.com', getdate()),
--   ('www.example6.com', getdate()),
--   ('www.example7.com', getdate());

-- select * from [Buffer]