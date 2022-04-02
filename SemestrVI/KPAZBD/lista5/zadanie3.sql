declare @counter int;
set @counter=1;
while @counter < 5000 begin
  insert into Czytelnik values(
    cast((12345600000 + @counter) as char(11)), 
    'Nazwisko' + cast (@counter as char(5)),
    'Miasto' + cast (@counter as char(5)),
    getdate(),
    null
  )
  set @counter+=1;
end;

set @counter=1;
while @counter < 5000 begin
  insert into Ksiazka values(
    cast (@counter as varchar(20)),
    'Tytu³ ' + cast (@counter as char(5)),
    'Autor ' + cast (@counter as char(5)),
    rand()*(2022-1950+1)+1950,
    rand()*(200-50+1)+50,
    null
  )
  set @counter+=1;
end;

set @counter=1;
while @counter < 5000 begin
  insert into Egzemplarz values(
    'S00' + cast (@counter as char(5)),
    rand()*(5000)
  )
  set @counter+=1;
end;

set @counter=1;
while @counter < 5000 begin
  insert into Wypozyczenie values(
    rand()*(5000),
    rand()*(5000),
    getdate(),
    rand()*(31)
  )
  set @counter+=1;
end;
go

checkpoint;
go
dbcc dropcleanbuffers;
go
set statistics time on;
SELECT DISTINCT c.PESEL, c.Nazwisko
FROM Egzemplarz e
JOIN Ksiazka k ON e.Ksiazka_ID=k.Ksiazka_ID
JOIN Wypozyczenie w ON e.Egzemplarz_ID=w.Egzemplarz_ID
JOIN Czytelnik c ON c.Czytelnik_ID = w.Czytelnik_ID;
set statistics time off;
go

checkpoint;
go
dbcc dropcleanbuffers;
go
set statistics time on;
SELECT c.PESEL, c.Nazwisko
FROM Czytelnik c WHERE c.Czytelnik_ID IN
(SELECT w.Czytelnik_ID FROM Wypozyczenie w
JOIN Egzemplarz e ON e.Egzemplarz_ID=w.Egzemplarz_ID
JOIN Ksiazka k ON e.Ksiazka_ID=k.Ksiazka_ID)
set statistics time off;
go

checkpoint;
go
dbcc dropcleanbuffers;
go
set statistics time on;
SELECT c.PESEL, c.Nazwisko
FROM Czytelnik c WHERE c.Czytelnik_ID IN
(SELECT w.Czytelnik_ID FROM Wypozyczenie w, Egzemplarz e, Ksiazka k 
WHERE e.Ksiazka_ID = k.Ksiazka_ID AND e.Egzemplarz_ID = w.Egzemplarz_ID)
set statistics time off;
go