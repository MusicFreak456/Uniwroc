select tables.object_id, tables.name, columns.name 
  from tempdb.sys.tables
	join tempdb.sys.columns
	on tables.object_id = columns.object_id
	where columns.name = 'characteristic_name'
go
	
select * from tempdb.INFORMATION_SCHEMA.tables
go
