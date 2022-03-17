drop table if exists S1;
drop table if exists M1;
drop table if exists S2;
drop table if exists M2;

create table M1 (
    K int primary key,
    V varchar(20)
);

insert into M1 values
    (1, 'foo'),
    (2, 'bar');

create table S1 (
    K int primary key,
    MFK int references M1(K),
    V varchar(20)
);

insert into S1 values
    (1, 1, 'foo'),
    (2, 2, 'bar'); -- to się uda

insert into S1 values
    (3, 3, 'foo'); -- to się nie uda,
    -- bo nie ma wiersza w M1 z K=3

delete from M1 where K = 2 -- to też nie, 
-- bo wiersz z S1 się odnosi do tego wiersza 

create table M2 (
    K1 int,
    K2 int,
    V varchar(20),
    primary key (K1, K2)
);

create table S2 (
    K int primary key,
    MFK1 int,
    MFK2 int,
    V varchar(20),
    foreign key (MFK1, MFK2) references M2(K1, K2)
);

-- analogiczne przykłady
insert into M2 values
    (1, 1, 'foo'),
    (2, 1, 'bar');

insert into S2 values
    (1, 1, 1, 'foo'),
    (2, 2, 1, 'bar'); -- to się uda

insert into S2 values
    (3, 2, 2, 'foo'); -- to się nie uda

delete from M2 where K1 = 2 and K2 = 1 -- to też nie

drop table if exists S1;

create table S1 (
    K int primary key,
    MFK int,
    V varchar(20)
    foreign key (MFK) references M1(K)
        on delete cascade
        on update set null
);

insert into S1 values
    (1, 1, 'foo'),
    (2, 2, 'bar');

delete from M1 where K = 2; -- wiersz z S2
-- odwołujący się do tego wpisu też zostanie
-- usunięty

update M1 set V='bar' where K = 1; -- to nic
-- nie zrobi

update M1 set K=2 where K = 1; -- wiersz odnoszący
-- się do to tego wiersza dostanie nulla w miejscu
-- klucza obcego

drop table if exists S1;

create table S1 (
    K int primary key,
    MFK int,
    V varchar(20)
    foreign key (MFK) references M1(K)
        on delete no action
);

insert into S1 values
    (1, 1, 'foo'),
    (2, 2, 'bar');

delete from M1 where K = 2; -- nie uda się