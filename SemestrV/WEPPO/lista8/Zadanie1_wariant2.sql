create type public.gender as enum('male', 'female', 'other');
create table public.person (
	id serial primary key,
	name varchar(30) not null,
	surname varchar(30) not null,
	gender gender not null
);

insert into public.person(name, surname, gender) values
('Cezary', 'Świtała', 'male'),
('Jan', 'Kowalski', 'male'),
('Anna', 'Nowak', 'female');

select * from public.person;