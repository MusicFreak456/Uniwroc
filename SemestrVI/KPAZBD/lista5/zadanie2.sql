-- 1 NF

drop table if exists Wizyta;
create table Wizyta (
  ID int identity(1,1) primary key,
  PacjentImie varchar(30),
  PacjentNazwisko varchar(30),
  AdresUlica varchar(30),
  AdresNrDomu int,
  AdresKodPocztowy char(6),
  AdresMiejscowosc varchar(40),
  [Data] datetime,
  Pokoj int,
  Kwota int,
  LekarzImie varchar(30),
  LekarzNazwisko varchar(30),
  Rodzaj varchar(20),
  Powod text
);

insert into Wizyta values
  ('Jan', 'Kot', 'Dolna', 6, '44-444', 'Bór', '2029-02-01 12:30', 12, 300, 'Marek', 'Ząbek', 
   'Dentystyczny', 'założenie protezy w (...)'),
  ('Maria', 'Mysz', 'Górna', 9, '55-555', 'Las', '2030-01-04 11:45', 7, 150, 'Ewa', 'Blacka', 
   'Dermatologiczny', 'oględziny znamiona (...)');

-- 2 NF

drop table if exists Wizyta, Pacjent;

create table Pacjent (
  ID int identity(1,1) primary key,
  Imie varchar(30),
  Nazwisko varchar(30),
  AdresUlica varchar(30),
  AdresNrDomu int,
  AdresKodPocztowy char(6),
  AdresMiejscowosc varchar(40),
)

create table Wizyta (
  ID int identity(1,1) primary key,
  PacjentID int references Pacjent(ID),
  [Data] datetime,
  Pokoj int,
  Kwota int,
  LekarzImie varchar(30),
  LekarzNazwisko varchar(30),
  Rodzaj varchar(20),
  Powod text
);

-- 3 NF

drop table if exists Wizyta, Pacjent, KodPocztowy;

create table KodPocztowy (
  Kod char(6) primary key,
  Miejscowosc varchar(40)
);

create table Pacjent (
  ID int identity(1,1) primary key,
  Imie varchar(30),
  Nazwisko varchar(30),
  AdresUlica varchar(30),
  AdresNrDomu int,
  AdresKodPocztowy char(6) references KodPocztowy(Kod)
);

create table Wizyta (
  ID int identity(1,1) primary key,
  PacjentID int references Pacjent(ID),
  [Data] datetime,
  Pokoj int,
  Kwota int,
  LekarzImie varchar(30),
  LekarzNazwisko varchar(30),
  Rodzaj varchar(20),
  Powod text
);