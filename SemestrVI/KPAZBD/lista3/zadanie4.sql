drop procedure if exists count_days;
drop type if exists id_table_type;
go

create type id_table_type as table (czytelnik_id int);
go

create procedure count_days
  @ids id_table_type readonly
  as
    select ids.czytelnik_id, sum(Liczba_Dni) as suma_dni 
    from @ids as ids
    join Wypozyczenie 
      on ids.czytelnik_id = Wypozyczenie.Czytelnik_ID
    group by ids.czytelnik_id
go

declare @ids id_table_type;
insert into @ids values (1), (2);
exec count_days @ids;