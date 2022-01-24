create sequence person_id_seq start 1;

create type public.gender as enum('male', 'female', 'other');
create table public.person (
	id integer primary key,
	name varchar(30) not null,
	surname varchar(30) not null,
	gender gender not null
);

insert into public.person values
(nextval('person_id_seq'),'Cezary', 'Świtała', 'male'),
(nextval('person_id_seq'),'Jan', 'Kowalski', 'male'),
(nextval('person_id_seq'),'Anna', 'Nowak', 'female');

select * from public.person;