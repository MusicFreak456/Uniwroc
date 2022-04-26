drop function if exists holds_at_least_for_days
go

create function holds_at_least_for_days(@days int) returns table
  return (
    select PESEL, count(Egzemplarz_ID) as LiczbaEgzemplarzy 
    from dbo.Czytelnik
    join Wypozyczenie 
      on Wypozyczenie.Czytelnik_ID = Czytelnik.Czytelnik_ID
  	where dateadd(day, @days, Wypozyczenie.[Data]) <= cast(getdate() as date)
    group by PESEL
  )
go
 
select * from holds_at_least_for_days(100)
