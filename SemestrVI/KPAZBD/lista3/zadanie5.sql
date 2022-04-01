-- tworzymy zmienną tablicową
declare @test table (characteristic_name int);
insert into @test select Czytelnik_ID from Czytelnik;

waitfor delay '00:00:05' -- w tym czasie można odpytać tempdb

select * from @test -- tutaj zadziała
go

select * from @test -- tutaj już nie
go

create table #Test(characteristic_name int);
insert into #Test select Czytelnik_ID from Czytelnik;

waitfor delay '00:00:05' -- w tym czasie można odpytać tempdb

select * from #Test -- tutaj zadziała

go

select * from #Test -- tutaj też!

go

create table ##Test(characteristic_name int);
insert into ##Test select Czytelnik_ID from Czytelnik;

waitfor delay '00:00:05' -- w tym czasie można odpytać tempdb

select * from ##Test -- tutaj zadziała

go

select * from ##Test -- tutaj też!