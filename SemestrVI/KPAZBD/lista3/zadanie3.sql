drop procedure if exists create_reader;
drop procedure if exists validate_pesel;
go

create procedure validate_pesel
  @PESEL char(11),
  @pesel_day int output,
  @pesel_month int output,
  @pesel_year int output
  as

  if isnumeric(@PESEL) = 0
    throw 50000, 'Pesel może się składać tylko z liczb.', 0;

  if len(@PESEL) != 11
    throw 50000, 'Pesel musi mieć 11 cyfr.', 0;

  declare @weights table (id int identity(1,1), weight int);
  insert into @weights values 
    (1), (3), (7), (9), (1), (3), (7), (9), (1), (3);
  
  declare @control_sum int;
  select @control_sum = 
      10 - sum(substring(@PESEL,id,1) * [weight]) % 10
    from @weights
  set @control_sum = iif(@control_sum = 10, 0, @control_sum)
  
  if @control_sum != substring(@PESEL, 11, 1)
    throw 50000, 'Pesel ma niepoprawną sumę kontrolną.', 0;

  set @pesel_year = substring(@PESEL, 1, 2);
  set @pesel_month = substring(@PESEL, 3, 2);
  set @pesel_day = substring(@PESEL, 5, 2);

  if @pesel_month > 20 begin
    set @pesel_year += 2000
    set @pesel_month -= 20
  end else begin 
    set @pesel_year += 1900
  end

go

create procedure create_reader 
  @PESEL char(11) = null,
  @Nazwisko varchar(30) = null,
  @Miasto varchar(30) = null,
  @Data_Urodzenia date = null,
  @Ostatnie_Wypozyczenie date = null 
  as

  declare @pesel_day int;
  declare @pesel_month int;
  declare @pesel_year int;

  if @PESEL is not null begin
    exec validate_pesel 
      @PESEL, 
      @pesel_day output, 
      @pesel_month output, 
      @pesel_year output;

    if @Data_Urodzenia is not null and (
      year(@Data_Urodzenia) != @pesel_year   or
      month(@Data_Urodzenia) != @pesel_month or
      day(@Data_Urodzenia) != @pesel_day )
       throw 50000, 'Pesel nie zgadza się z datą urodzenia.', 0;
  end

  if @Nazwisko is not null begin
    if len(@Nazwisko) < 2 
      throw 50000, 'Nazwisko musi składać się z co najmniej 2 liter.', 0;
    if @Nazwisko not like '[A-Z][a-zA-Z]%'
      throw 50000, 'Nazwisko musi zaczynać się od wielkiej litery.', 0;
  end

  insert into Czytelnik values 
    (@PESEL, @Nazwisko, @Miasto, @Data_Urodzenia, @Ostatnie_Wypozyczenie);
go

exec create_reader 
  @PESEL='05212155298',
  @Data_Urodzenia='2005-1-21',
  @Nazwisko='Nazwisko'
