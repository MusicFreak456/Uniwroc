create table public.workplace (
	id serial primary key,
	workplace_name varchar(200) not null
);

create type public.gender as enum('male', 'female', 'other');
create table public.person (
	id serial primary key,
	name varchar(30) not null,
	surname varchar(30) not null,
	gender gender not null,
	id_workplace int,
	constraint fk_workplace
		foreign key(id_workplace)
			references workplace(id)
);
