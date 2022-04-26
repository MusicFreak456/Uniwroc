drop table if exists imiona
drop table if exists nazwiska
drop table if exists dane

create table imiona (
  id int identity,
  imie name unique
)

create table nazwiska (
  id int identity,
  nazwisko name unique
)

create table dane (
  imie name,
  nazwisko name,
  primary key (imie, nazwisko)
)

insert into imiona(imie) values 
  ('Sebastian'), ('Jozefat'), ('Przemysław')

insert into nazwiska(nazwisko) values
  ('Czuły'), ('Skała'), ('Kowalski')
go

drop procedure if exists random_pairs
go

create procedure random_pairs @n int as
  declare @pairs table(imie name, nazwisko name, primary key (imie, nazwisko));
  insert into @pairs select imie, nazwisko from imiona, nazwiska;

  if @@ROWCOUNT / 2 < @n
    throw 50000, 'argument "n" większy od połowy liczby możliwości', 0;
  
  truncate table dane;

  insert into dane 
    select top(@n) * from @pairs
    order by newid()
go

exec random_pairs @n=4