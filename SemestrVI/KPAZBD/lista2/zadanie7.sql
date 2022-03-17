drop table if exists Test;

create table Test (
    id int identity(1000,10)
)

declare @cnt int = 0;

while @cnt < 5
begin
   insert into Test default values;
   set @cnt = @cnt + 1;
end;

select * from Test;

select @@identity; -- zwraca ostatnie id 
-- wygenerowane w ramach tego połączenia
select ident_current('Test'); -- zwraca ostatnie id
-- wygenerowane dla tej tabeli w ramach wszystkich 
-- połączeń (czyli możemy dodać coś z innego i to się 
-- zmieni, można pokazać np. DBeaverem)