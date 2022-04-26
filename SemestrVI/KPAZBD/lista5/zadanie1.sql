drop table if exists Test;

create table Test (
  quantity int
);

insert into Test values (1);

begin transaction;
  update Test set quantity = 2;
  waitfor delay '00:00:10';
rollback;

-- brudny odczyt:
-- set transaction isolation level read uncommitted; select * from Test; 
-- brak brudnego odczytu:
-- set transaction isolation level read committed; select * from Test; 
-- set transaction isolation level repeatable read; select * from Test; 
-- set transaction isolation level serializable; select * from Test;

-- Niepowtarzalne odczyty:
-- set transaction isolation level read uncommitted;
-- set transaction isolation level read committed;
-- Brak niepowtarzalnych odczytów:
-- set transaction isolation level repeatable read;
-- set transaction isolation level serializable;
begin transaction;
  declare @quantity int;
  
  select @quantity=quantity from Test;
  print @quantity;

  waitfor delay '00:00:10';

  select @quantity=quantity from Test;
  print @quantity;
commit;

begin transaction;
  declare @quantity int;
  select @quantity=quantity from Test;
  update Test set quantity = @quantity + 1;
commit;


-- Odczyty fantomów:
-- set transaction isolation level read uncommitted;
-- set transaction isolation level read committed;
-- set transaction isolation level repeatable read;
-- Brak odczytów fantomów:
-- set transaction isolation level serializable;
begin transaction;
  declare @sum int;
  select @sum = sum(quantity) from Test;
  print @sum;

  waitfor delay '00:00:10';

  select @sum = sum(quantity) from Test;
  print @sum;
commit

begin transaction;
  insert into Test values (1)
commit;